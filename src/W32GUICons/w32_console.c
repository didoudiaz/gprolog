/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 GUI console                                               *
 * File  : w32_console.c                                                   *
 * Descr.: W32 GUI Console                                                 *
 * Author: Jacob Navia and Daniel Diaz                                     *
 *                                                                         *
 * Copyright (C) 1999-2003 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2, or any later version.       *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

#include <stdio.h>
#include <string.h>
#include <malloc.h>

#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <richedit.h>
#include <shlobj.h>
#include <shellapi.h>

#include <htmlhelp.h>

#include "w32_resource.h"

#include "../TopComp/prolog_path.c"


#if 1
#define DLLEXPORT __declspec(dllexport)
#endif

#if 0				/* avoid to link with htmlhelp.lib - needs hhctrl.ocx at runtime */
#define LOAD_HTMLHELP_DYNAMICALLY
#endif



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define EDIT_FIELD_SIZE            32000  /* 64 Kb for XP, 32 Kb for Win98 ? */

#define ABOUT_TEXT \
    PROLOG_NAME " version " PROLOG_VERSION "\n" \
    "By Daniel Diaz\n" \
    PROLOG_COPYRIGHT "\n\n" \
    "http://gprolog.inria.fr\n\n" \
    PROLOG_NAME " comes with ABSOLUTELY NO WARRANTY.\n" \
    "You may redistribute copies of " PROLOG_NAME " under the\n" \
    "terms of the GNU General Public License."


/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/


/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static char *(*fct_get_separators) ();
static int (*fct_get_prompt_length) ();

static unsigned int queue[10240];
static int queue_start, queue_end;
static CRITICAL_SECTION cs;
static HANDLE event_char_in_queue;


static HWND hwndMain;		// Main window handle
static HWND hwndEditControl;
static WNDPROC lpEProc;
static HINSTANCE hInst; 	// Instance handle
static LOGFONT CurrentFont;
static HFONT hCourier;

static int show_console = 0; // is the associated text console shown ?
static HWND hwnd_console;

static int in_get_char = 0;
static int last_is_read = 0; // to know if a msg box to display at exit
static int posit = 0;		 // position inside current (last) line
static int line_buffering = 1;	// default: line buffered
static char wr_buffer[4096];
static char *wr_buffer_ptr = wr_buffer;



/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int CallMain(void *unused);

static int StartWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                        LPSTR lpCmdLine, INT nCmdShow);

static BOOL InitApplication(void);

static HWND Createw32_consoleWndClassWnd(void);

static LRESULT CALLBACK MainWndProc(HWND hwnd, UINT msg, WPARAM wParam,
                                    LPARAM lParam);

static void MainWndProc_OnCommand(HWND hwnd, int id, HWND hwndCtl,
								  UINT codeNotify);



static HFONT CreationCourier(int flag);

static int CallChangeFont(HWND hwnd);



static void SubClassEditField(HWND hwnd);

static LRESULT CALLBACK SubClassEdit(HWND hwnd, UINT msg, WPARAM mp1,
                                     LPARAM mp2);



static char *Get_Current_Word(void);

static void Show_Help(char *word);

static int Get_CHM_Help_Path(char *path);

static int Browse_For_Dir(char *result);

static int WINAPI BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp,
                                     LPARAM pData);



static void Add_Clipboard_To_Queue(void);

static void Add_Char_To_Queue(int c);

static void Set_Selection(int posit, int n);

static void Set_Caret_Position(int posit);

static void Move_Caret_From_Mouse(void);

static void Display_Text(char *str, int n);

static void Flush_Buffer(void);

static int Console_Printf(char *format, ...);


DLLEXPORT void W32GC_Set_Line_Buffering(int is_buffered);
DLLEXPORT void W32GC_Backd(int n);
DLLEXPORT int W32GC_Confirm_Box(char *titre, char *msg);


static BOOL Launched_From_Command_Line();
static HWND Find_Text_Console_Handle(void);
static void Show_Text_Console(int show);


/*<---------------------------------------------------------------------->*/
#ifdef __LCC__
#define DllMain LibMain
#endif

BOOL DLLEXPORT WINAPI
DllMain(HINSTANCE hDLLInst, DWORD fdwReason, LPVOID lpvReserved)
{
    switch (fdwReason)
    {
    case DLL_PROCESS_ATTACH:
        // The DLL is being loaded for the first time by a given process.
        // Perform per-process initialization here.  If the initialization
        // is successful, return TRUE; if unsuccessful, return FALSE.
        hInst = hDLLInst;
        break;
        
    case DLL_PROCESS_DETACH:
        // The DLL is being unloaded by a given process.  Do any
        // per-process clean up here, such as undoing what was done in
        // DLL_PROCESS_ATTACH.  The return value is ignored.
        break;
        
    case DLL_THREAD_ATTACH:
        // A thread is being created in a process that has already loaded
        // this DLL.  Perform any per-thread initialization here.  The
        // return value is ignored.
        break;
        
    case DLL_THREAD_DETACH:
        // A thread is exiting cleanly in a process that has already
        // loaded this DLL.  Perform any per-thread clean up here.  The
        // return value is ignored.
        break;
    }
    return TRUE;
}


/* get_separators can be NULL (default separators are used)
 * get_prompt_length can be NULL (prompt length supposed to be 0)
 *    only relevant when the user clicks inside the last line inside the prompt
 *    having the length we can consider it similarly to when he clicks above
 *    (to set the cursor for a copy/paste).
 *    not having the length we try to move the cursor (sending left arrows)
 *    the application (e.g. linedit) then needs to detect an invalid left arrow
 *    and ignore it (e.g. emit a beep). Anyway the user can copy/paste the prompt.
 *    So NULL is really OK.
 */
DLLEXPORT int
W32GC_Start_Window(char *(*get_separators) (), int (*get_prompt_length) ())
{
    int tid;
    
    fct_get_separators = get_separators;
    fct_get_prompt_length = get_prompt_length;
    hwnd_console = Find_Text_Console_Handle();
    show_console = Launched_From_Command_Line();
    Show_Text_Console(show_console);
    
    InitializeCriticalSection(&cs);
    CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) CallMain, hwndMain, 0,
	       &tid);
    Sleep(500); // tempo to wait windows manager ok in the thread
    return 1;
}


static int
CallMain(void *unused)
{
    StartWinMain(hInst, 0, "", SW_SHOW);
    return 0;
}


static int
StartWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine,
             INT nCmdShow)
{
    MSG msg;
    HANDLE hAccelTable;
    
    hInst = hInstance;
    event_char_in_queue = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (!InitApplication())
        return 0;
    hAccelTable = LoadAccelerators(hInst, MAKEINTRESOURCE(IDR_ACCEL));
    if ((hwndMain = Createw32_consoleWndClassWnd()) == (HWND) 0)
        return 0;
    ShowWindow(hwndMain, SW_SHOW);
    while (GetMessage(&msg, NULL, 0, 0))
    {
        if (!TranslateAccelerator(hwndMain, hAccelTable, &msg))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }
    CloseHandle(event_char_in_queue);
    DeleteCriticalSection(&cs);
    
    return msg.wParam;
}



static BOOL
InitApplication(void)
{
    WNDCLASS wc;
    
    memset(&wc, 0, sizeof(WNDCLASS));
    
    wc.style = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
    wc.lpfnWndProc = (WNDPROC) MainWndProc;
    wc.hInstance = hInst;
    wc.hbrBackground = (HBRUSH) (COLOR_WINDOW + 1);
    wc.lpszClassName = "w32_consoleWndClass";
    wc.lpszMenuName = MAKEINTRESOURCE(IDR_MENU);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hIcon = LoadIcon(NULL, IDI_WINLOGO);
    if (!RegisterClass(&wc))
        return 0;
    return 1;
}

static HWND
Createw32_consoleWndClassWnd(void)
{
    return CreateWindow("w32_consoleWndClass", "GNU Prolog console",
		      WS_MINIMIZEBOX | WS_VISIBLE | WS_CLIPSIBLINGS |
              WS_CLIPCHILDREN | WS_MAXIMIZEBOX | WS_CAPTION |
              WS_BORDER | WS_SYSMENU | WS_THICKFRAME, CW_USEDEFAULT,
              0, CW_USEDEFAULT, 0, NULL, NULL, hInst, NULL);
}


static LRESULT CALLBACK
MainWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    RECT rc;
    PAINTSTRUCT ps;
    static HWND hwndChild;
    
    switch (msg)
    {
    case WM_CREATE:
        GetClientRect(hwnd, &rc);
        hwndChild = CreateWindow("EDIT",
            NULL,
            WS_CHILD | WS_VISIBLE |
            ES_MULTILINE |
            WS_VSCROLL | WS_HSCROLL |
            ES_AUTOHSCROLL | ES_AUTOVSCROLL,
            0,
            0,
            (rc.right - rc.left),
            (rc.bottom - rc.top),
            hwnd, (HMENU) 1000, hInst, NULL);
        SetWindowLong(hwnd, DWL_USER, (DWORD) hwndChild);
        hCourier = CreationCourier(1);
        SendMessage(hwndChild, WM_SETFONT, (WPARAM) hCourier, 0L);
        SubClassEditField(hwndChild);
        hwndEditControl = hwndChild;
		SendMessage(hwndEditControl, EM_SETLIMITTEXT, EDIT_FIELD_SIZE, 0);
        SetFocus(hwndChild);
        break;
        
    case WM_SIZE:
        GetWindowRect(hwnd, &rc);
        MoveWindow(hwndChild, 0, 0, LOWORD(lParam), HIWORD(lParam), 1);
        break;
    case WM_SETFOCUS:
        SetFocus(hwndEditControl);
        break;
    case WM_COMMAND:
        HANDLE_WM_COMMAND(hwnd, wParam, lParam, MainWndProc_OnCommand);
        break;
#if 0
    case WM_CLOSE:
        if (!W32GC_Confirm_Box("GNU Prolog", "Really quit ?"))
            return 0;
        return DefWindowProc(hwnd, msg, wParam, lParam);
#endif
    case WM_PAINT:
        BeginPaint(hwnd, &ps);
        EndPaint(hwnd, &ps);
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        exit(0);
        break;
    default:
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    
    return 0;
}

static void
MainWndProc_OnCommand(HWND hwnd, int id, HWND hwndCtl, UINT codeNotify)
{
    switch (id)
    {
    case IDM_EXIT:
        PostMessage(hwnd, WM_CLOSE, 0, 0);
        break;
        
        
    case IDM_COPY:
        SendMessage(hwndEditControl, WM_COPY, 0, 0);
        break;
        
    case IDM_PASTE:
        Add_Clipboard_To_Queue();
        break;
        
        
    case IDM_INTERRUPT:
        if (in_get_char)
            Add_Char_To_Queue(3);	/* i.e. CTRL+C */
        else
            GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
        break;
        
    case IDM_BUFFERING:
        line_buffering = 1 - line_buffering;
        W32GC_Set_Line_Buffering(line_buffering);
        break;
        
    case IDM_FLUSH:
        Flush_Buffer();
        break;
        
    case IDM_SHOW_CONSOLE:
        show_console = 1 - show_console;
        Show_Text_Console(show_console);
        SetFocus(hwndEditControl);
        break;
        
        
    case IDM_FONT:
        CallChangeFont(hwnd);
        break;
        
    case IDM_MANUAL:
        Show_Help(NULL);
        break;
        
    case IDM_INDEX:
        Show_Help("");
        break;
        
    case IDM_ABOUT:
        MessageBox(hwndMain, ABOUT_TEXT, "About GNU Prolog",
            MB_OK | MB_ICONINFORMATION);
        break;
    }
}



static HFONT
CreationCourier(int flag)
{
    memset(&CurrentFont, 0, sizeof(LOGFONT));
    
    CurrentFont.lfCharSet = ANSI_CHARSET;
    CurrentFont.lfWeight = FW_NORMAL;
    if (flag)
        CurrentFont.lfHeight = 18;
    else
        CurrentFont.lfHeight = 15;
    CurrentFont.lfPitchAndFamily = (BYTE) (FIXED_PITCH | FF_MODERN);
    strcpy(CurrentFont.lfFaceName, "Courier");	/* Courier */
    return (CreateFontIndirect(&CurrentFont));
}


static int
CallChangeFont(HWND hwnd)
{
    LOGFONT lf;
    CHOOSEFONT cf;
    int r;
    
    memset(&cf, 0, sizeof(CHOOSEFONT));
    memcpy(&lf, &CurrentFont, sizeof(LOGFONT));
    cf.lStructSize = sizeof(CHOOSEFONT);
    
    cf.hwndOwner = hwnd;
    cf.lpLogFont = &lf;
    cf.Flags =
        CF_SCREENFONTS | CF_EFFECTS | CF_APPLY | CF_INITTOLOGFONTSTRUCT;
    cf.nFontType = SCREEN_FONTTYPE;
    r = ChooseFont(&cf);
    if (!r)
        return (0);
    memcpy(&CurrentFont, &lf, sizeof(LOGFONT));
    
    hCourier = CreateFontIndirect(&CurrentFont);
    SendMessage(hwndEditControl, WM_SETFONT, (WPARAM) hCourier, 0);
    InvalidateRect(hwndEditControl, NULL, 1);
    SendMessage(hwndEditControl, EM_SCROLLCARET, 0, 0);
    return (1);
}

/*<---------------------------------------------------------------------->*/

static void
SubClassEditField(HWND hwnd)
{
    if (lpEProc == NULL)
        lpEProc = (WNDPROC) GetWindowLong(hwnd, GWL_WNDPROC);
    SetWindowLong(hwnd, GWL_WNDPROC, (DWORD) SubClassEdit);
}

static LRESULT CALLBACK
SubClassEdit(HWND hwnd, UINT msg, WPARAM mp1, LPARAM mp2)
{
    LRESULT r;
    int c, repeat, hasCtrl, hasAlt;
    unsigned char pKeyState[256];
    
    if (msg == WM_CHAR)
    {
        repeat = (int) (mp2 & 0xffff);
        while (repeat--)
            Add_Char_To_Queue(mp1);
        return 0;
    }
    if (msg == WM_KEYDOWN)
    {
        GetKeyboardState(pKeyState);
        hasCtrl = (pKeyState[VK_CONTROL] & (unsigned char) 0x80);
        hasAlt = (pKeyState[VK_MENU] & (unsigned char) 0x80);
        
#if 0				/* now done by accelerators defined in resources */
        if ((mp1 == 'c' || mp1 == 'C') && hasCtrl && hasAlt)
        {
            SendMessage(hwndMain, WM_COMMAND, IDM_COPY, 0);
            return 0;
        }
        if ((mp1 == 'v' || mp1 == 'V') && hasCtrl && hasAlt)
        {
            SendMessage(hwndMain, WM_COMMAND, IDM_PASTE, 0);
            return 0;
        }
#endif
        c = 0;
        switch (mp1)
        {
        case VK_NEXT:		/* default vertical scroll behavior */
        case VK_PRIOR:
            break;
            
        case VK_LEFT:
        case VK_RIGHT:
        case VK_UP:
        case VK_DOWN:
        case VK_HOME:
        case VK_END:
        case VK_INSERT:
        case VK_DELETE:
            c = (hasCtrl) ? 2 : 1;
            c = ((c << 8) | mp1);
            Add_Char_To_Queue(c);
            return 0;
            
        case VK_F1:		/* not done by an accelerator for contextual help */
            Show_Help(Get_Current_Word());
            return 0;
			
#if 0 /* to include a test code */
        case VK_F2:
			{
				int size = SendMessage(hwndEditControl, EM_GETLIMITTEXT, 0, 0);
				int len = SendMessage(hwndEditControl, WM_GETTEXTLENGTH, 0, 0);
				char s[100];
				
				int beg,end;
				Set_Selection(3, 200);
				SendMessage(hwndEditControl, EM_GETSEL, &beg, &end);
				
				sprintf(s,"limit: %d   len: %d   sel: %d-%d", size, len, beg, end);
				MessageBox(NULL, s, "Error", MB_OK);
				// size += 10;
				SendMessage(hwndEditControl, EM_SETLIMITTEXT, size, 0);
				return 0;
			}
#endif        
		}
        
    }
    r = CallWindowProc(lpEProc, hwnd, msg, mp1, mp2);
    if (msg == WM_LBUTTONDOWN)	/* left button (inside cur line) move the caret */
        Move_Caret_From_Mouse();
    
    return r;
}

/*<---------------------------------------------------------------------->*/
#define Is_A_Sep(c)  (strchr(separators, c) != NULL)

static char *
Get_Current_Word(void)
{
    static char *buff = NULL;
    static int buff_len = 0;
    char *p, *q;
    int start, end, line_idx, length, offset;
    char *separators = " ,;:-'\"!@$#^&()-+*/\\[]|<=>`~{}";
    
    if (fct_get_separators)
        separators = (*fct_get_separators) ();
    
    SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &start, (LPARAM) &end);
    line_idx = SendMessage(hwndEditControl, EM_LINEFROMCHAR, start, 0);
    length = SendMessage(hwndEditControl, EM_LINELENGTH, start, 0);
    offset = SendMessage(hwndEditControl, EM_LINEINDEX, line_idx, 0);
    if (buff_len < length)
    {
        buff_len = length + 128;
        buff = (char *) realloc(buff, buff_len);
    }
    
    *(unsigned short *) buff = length;
    SendMessage(hwndEditControl, EM_GETLINE, line_idx, (LPARAM) buff);
    buff[length] = '\0';
    p = q = buff + (start - offset);
    if (!Is_A_Sep(*p))		/* else an empty word */
    {
        while (p >= buff && !Is_A_Sep(*p))
            p--;
        p++;
        while (*q && !Is_A_Sep(*q))
            q++;
    }
    *q = '\0';
    return p;
}


static void
Show_Help(char *word)
{
    
    char help_path[1024];
    HWND hwnd = 0;		// or GetDesktopWindow();
    UINT command;
    HH_AKLINK link;
    DWORD data;
    
#ifdef LOAD_HTMLHELP_DYNAMICALLY
    typedef HWND (WINAPI *FHH) ();
    HINSTANCE inst;
    static FHH HtmlHelp;
    
    if (HtmlHelp == NULL &&
        ((inst = LoadLibrary("hhctrl.ocx")) == NULL ||
        (HtmlHelp = (FHH) GetProcAddress(inst, "HtmlHelpA")) != NULL))
    {
        MessageBox(NULL, "Error loading hhctrl.ocx / HtmlHelpA", "Error",
            MB_OK);
        return;
    }
#endif
    if (!Get_CHM_Help_Path(help_path))	/* if CANCEL, abort */
        return;
    
    if (word == NULL)		/* open first page of the manual */
    {				/* use strcat(help_path, "::/file.html#target") to open a specific page+target */
        command = HH_DISPLAY_TOPIC;
        data = 0;
    }
    else
    {
        link.cbStruct = sizeof(HH_AKLINK);
        
        link.fReserved = FALSE;
        link.pszKeywords = word;
        link.pszUrl = NULL;	// or .chm://index.html ?
        link.pszMsgText = NULL;
        link.pszMsgTitle = NULL;
        link.pszWindow = NULL;
        link.fIndexOnFail = TRUE;
        command = HH_KEYWORD_LOOKUP;
        data = (DWORD) &link;
    }
    
    if (HtmlHelp(hwnd, help_path, command, data) == 0)
        MessageBox(NULL, help_path, "HtmlHelp Error", MB_OK);
}


static int
Get_CHM_Help_Path(char *path)
{
    char *p;
    int devel_mode;
    
    for (;;)
    {
        if ((p = Get_Prolog_Path(&devel_mode)) != NULL)
            break;
        
        if (Browse_For_Dir(path) == 0)
            return 0;
        
        Read_Write_Registry(0, "RootPath", path, 0);
    }
    
#if 0				/* remove the stored path to enforce Browse_For_Dir next time (debug only) */
    MessageBox(NULL, p, "Prolog Root Path", MB_OK);
    Read_Write_Registry(0, "RootPath", "", 0);
#endif
    
    if (devel_mode)
        sprintf(path, "%s\\..\\..\\doc\\manual.chm", p);
    else
        sprintf(path, "%s\\doc\\manual.chm", p);
    return 1;
}



static int
Browse_For_Dir(char *result)
{
    LPMALLOC pMalloc;
    BROWSEINFO browseInfo;
    LPITEMIDLIST lpItemIDList;
    int r = 0;
    char *Title = "GNU Prolog";
    
    CoInitialize(0);
    if (S_OK != SHGetMalloc(&pMalloc))
        return 0;
    
    memset(&browseInfo, 0, sizeof(BROWSEINFO));
    
    browseInfo.hwndOwner = GetActiveWindow();
    browseInfo.lpszTitle = Title;
    browseInfo.lpfn = BrowseCallbackProc;
    browseInfo.ulFlags = BIF_STATUSTEXT | 0x40;	// use the new user interface
    lpItemIDList = SHBrowseForFolder(&browseInfo);
    if (lpItemIDList != NULL)
    {
        *result = 0;
        if (SHGetPathFromIDList(lpItemIDList, result))
        {
            if (result[0])
            {
                r = 1;
            }
        }
        pMalloc->lpVtbl->Free(pMalloc, lpItemIDList);
    }
    pMalloc->lpVtbl->Release(pMalloc);
    CoUninitialize();
    return r;
}

static int WINAPI
BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData)
{
    char szDir[MAX_PATH];
    
    switch (uMsg)
    {
    case BFFM_INITIALIZED:
        {
            if (GetCurrentDirectory(sizeof(szDir), szDir))
            {
                // WParam is TRUE since you are passing a path.
                // It would be FALSE if you were passing a pidl.
                SendMessage(hwnd, BFFM_SETSELECTION, TRUE, (LPARAM) szDir);
            }
            break;
        }
    case BFFM_SELCHANGED:
        {
            // Set the status window to the currently selected path.
            if (SHGetPathFromIDList((LPITEMIDLIST) lp, szDir))
            {
                SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, (LPARAM) szDir);
            }
            break;
        }
    default:
        break;
    }
    return 0;
}

/*<---------------------------------------------------------------------->*/

static void
Add_Clipboard_To_Queue(void)
{
    if (IsClipboardFormatAvailable(CF_TEXT) && OpenClipboard(hwndMain))
    {
        HANDLE hClipData = GetClipboardData(CF_TEXT);
        
        if (hClipData)
        {
            char *str = GlobalLock(hClipData);
            
            if (str)
                while (*str)
                {
					/* from terminal.h */
#define KEY_ESC(x)                 ((2<<8) | ((x)|0x20))
					
					if (*str == '\t') /* send ESC+tab */
						Add_Char_To_Queue(KEY_ESC('\t'));
					else 
						if (*str != '\r')
							Add_Char_To_Queue(*str);
						str++;
                }
                GlobalUnlock(hClipData);
        }
        CloseClipboard();
    }
    
}


static void
Add_Char_To_Queue(int c)
{
    EnterCriticalSection(&cs);
    queue[queue_end] = c;
    queue_end++;
    if (queue_end >= sizeof(queue))
        queue_end = 0;
    LeaveCriticalSection(&cs);
    SetEvent(event_char_in_queue);
}


DLLEXPORT int
W32GC_Kbd_Is_Not_Empty()
{
    return queue_start != queue_end;
}

DLLEXPORT int
W32GC_Get_Char0()
{
    int result;
    
    in_get_char = 1;
    
    Flush_Buffer();		/* synchronize output and posit */
    
    last_is_read = 1;
    while (queue_start == queue_end)
    {
        WaitForSingleObject(event_char_in_queue, INFINITE);
    }
    
    EnterCriticalSection(&cs);
    result = queue[queue_start];
    queue_start++;
    if (queue_start >= sizeof(queue))
        queue_start = 0;
    LeaveCriticalSection(&cs);
    
    in_get_char = 0;
    last_is_read = 1;
    
    return result;
}




static void
Set_Selection(int posit, int n)
{
    int lines = SendMessage(hwndEditControl, EM_GETLINECOUNT, 0, 0);
    int line_index = SendMessage(hwndEditControl, EM_LINEINDEX, lines - 1, 0);
    
    SendMessage(hwndEditControl, EM_SETSEL, line_index + posit,
        line_index + posit + n);
}

static void
Set_Caret_Position(int posit)
{
    Set_Selection(posit, 0);
}



static void
Move_Caret_From_Mouse(void)
{
    int start, end;
    int lines = SendMessage(hwndEditControl, EM_GETLINECOUNT, 0, 0);
    int line_index = SendMessage(hwndEditControl, EM_LINEINDEX, lines - 1, 0);
    int prompt_length = 0;
    int c = (1 << 8) | VK_RIGHT;
    int count;
    
    if (fct_get_prompt_length)
        prompt_length = (*fct_get_prompt_length) ();
    
    SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &start, (LPARAM) &end);
    
    end -= line_index;		/* < 0 if not in the current (last) line */
    if (end < prompt_length)	/* not in cur line nor in the prompt: do nothing */
        return;
    
    count = end - posit;
    if (count < 0)
        
    {
        count = -count;
        c = (1 << 8) | VK_LEFT;
    }
    while (count--)
        Add_Char_To_Queue(c);
}



static void
Display_Text(char *str, int n)
{
    while (n--)
    {
        switch (*str)
        {
        case '\r':
            break;
            
        case '\n':
            Flush_Buffer();	/* emit the line */
            Set_Selection(0x7fff, 0);	/* move caret at end of line */
            SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) "\r\n");
            posit = 0;
            break;
            
        case '\b':		/* really needed if only used if W32GC_Backd not defined */
            W32GC_Backd(1);
            break;
            
        default:
            if (wr_buffer_ptr - wr_buffer >= sizeof(wr_buffer))
                Flush_Buffer();
            
            *wr_buffer_ptr++ = isprint(*str) ? *str : ' ';
        }
        str++;
    }
    if (!line_buffering)
        Flush_Buffer();
}


static void
Flush_Buffer(void)
{
    int n = wr_buffer_ptr - wr_buffer;
	int to_add = n + 2; /* maybe a \r\n after this */
    int max_size = SendMessage(hwndEditControl, EM_GETLIMITTEXT, 0, 0);
	int cur_size = SendMessage(hwndEditControl, WM_GETTEXTLENGTH, 0, 0);
    
    if (n == 0)
        return;
    
    *wr_buffer_ptr = '\0';
	
	if (n > 5) /* avoid this for short outputs, e.g. interactive reading with echo */
	{
		int start, end;
		Set_Selection(posit, n);
		SendMessage(hwndEditControl, EM_GETSEL, (LPARAM) &start, (LPARAM) &end);
		to_add -= (end - start);
	}
	
	
	if (cur_size + to_add > max_size) /* not enough space - remove some beginning lines */
	{
		int line = SendMessage(hwndEditControl, EM_LINEFROMCHAR, to_add, 0);
		int line_index = SendMessage(hwndEditControl, EM_LINEINDEX, line + 1, 0);
		SendMessage(hwndEditControl, EM_SETSEL, 0, line_index);
		SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) wr_buffer_ptr);  /* empty string to remove lines */
	}
	
    Set_Selection(posit, n);
    SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) wr_buffer);
    posit += n;
    wr_buffer_ptr = wr_buffer;
    last_is_read = 0;
}


DLLEXPORT void
W32GC_Set_Line_Buffering(int is_buffered)
{
    line_buffering = is_buffered;
    
    if (!line_buffering)
    {
        Flush_Buffer();
        CheckMenuItem(GetMenu(hwndMain), IDM_BUFFERING,
            MF_BYCOMMAND | MF_UNCHECKED);
        EnableMenuItem(GetMenu(hwndMain), IDM_FLUSH,
            MF_BYCOMMAND | MF_GRAYED);
    }
    else
    {
        CheckMenuItem(GetMenu(hwndMain), IDM_BUFFERING,
            MF_BYCOMMAND | MF_CHECKED);
        EnableMenuItem(GetMenu(hwndMain), IDM_FLUSH,
            MF_BYCOMMAND | MF_ENABLED);
    }
}


DLLEXPORT int
W32GC_Get_Line_Buffering(void)
{
    return line_buffering;
}



DLLEXPORT void
W32GC_Flush(FILE *f)
{
    Flush_Buffer();
}

#if 0
static int
Console_Printf(char *format, ...)	/* debugging purpose: display in the GUI */
{
    va_list arg_ptr;
    char buff[1024];
    int ret;
    
    va_start(arg_ptr, format);
    ret = vsprintf(buff, format, arg_ptr);
    Display_Text(buff, strlen(buff));
    va_end(arg_ptr);
    
    return ret;
}
#endif


DLLEXPORT void
W32GC_Put_Char(int c)
{
    char c1 = c;
    
    Display_Text(&c1, 1);
}



DLLEXPORT void
W32GC_Backd(int n)
{
    Flush_Buffer();		/* synchronize output and posit */
    posit -= n;
    Set_Caret_Position(posit);
}

DLLEXPORT void
W32GC_Forwd(int n)
{
    Flush_Buffer();		/* synchronize output and posit */
    posit += n;
    Set_Caret_Position(posit);
}

DLLEXPORT void
W32GC_Displ(int n, char *str)
{
    Display_Text(str, n);
}


DLLEXPORT void
W32GC_Displ_Str(char *str)
{
    Display_Text(str, strlen(str));
}


DLLEXPORT void
W32GC_Erase(int n)
{
    Flush_Buffer();		/* synchronize output and posit */
    Set_Selection(posit, n);
    SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) "");
}


DLLEXPORT void
W32GC_Emit_Beep()
{
    Beep(440, 100);
}



DLLEXPORT void
W32GC_Ins_Mode(int ins_mode)
{
}


DLLEXPORT void
W32GC_Screen_Size(int *row, int *col)
{
    HDC hDC;
    RECT rc;
    int nHautCar, nLargCar;
    TEXTMETRIC textmetric;
    HFONT oldFont;
    
    hDC = GetDC(hwndEditControl);
    oldFont = SelectObject(hDC, hCourier);
    GetTextMetrics(hDC, &textmetric);
    nHautCar = textmetric.tmExternalLeading + textmetric.tmHeight;
    nLargCar = textmetric.tmAveCharWidth;
    SelectObject(hDC, oldFont);
    ReleaseDC(hwndEditControl, hDC);
    GetClientRect(hwndEditControl, &rc);
    rc.bottom -= GetSystemMetrics(SM_CYHSCROLL);
    rc.right -= GetSystemMetrics(SM_CXVSCROLL);
    *col = rc.right / nLargCar;
    *row = rc.bottom / nHautCar;
}


DLLEXPORT int
W32GC_Confirm_Box(char *title, char *msg)
{
    UINT utype;
    
    if (IsIconic(hwndMain))
        ShowWindow(hwndMain, SW_RESTORE);
    
    Flush_Buffer();
    
    utype = MB_YESNO | MB_SETFOREGROUND | MB_ICONQUESTION;
    
    return (MessageBox(hwndMain, msg, title, utype) == IDYES);
}


DLLEXPORT void
W32GC_Message_Box(char *title, char *msg, int type)
{
    UINT utype;
    
    if (IsIconic(hwndMain))
        ShowWindow(hwndMain, SW_RESTORE);
    
    Flush_Buffer();
    
    utype = MB_OK | MB_SETFOREGROUND;
    if (type == 0)                    // error
        utype |= MB_ICONERROR;
    else if (type == 1)               // warning
        utype |= MB_ICONWARNING;
    else if (type == 2)
        utype |= MB_ICONINFORMATION;  // information
    else if (type == 3)
        utype |= MB_ICONQUESTION;     // question
    
    MessageBox(hwndMain, msg, title,utype);
}



DLLEXPORT void
W32GC_Exit_Process(int ret_val)
{
    Flush_Buffer();		/* synchronize output and posit */
    
    if (!last_is_read)
        W32GC_Message_Box("GNU Prolog", "Program terminated", 2);
}



static HWND
Find_Text_Console_Handle(void)
{
    HWND hwnd;
    char save_title[256];
    char uniq_title[256];
    
    GetConsoleTitle(save_title, sizeof(save_title));
    sprintf(uniq_title, "%d/%d", GetTickCount(), GetCurrentProcessId());
    SetConsoleTitle(uniq_title);
    Sleep(40); // wait to be sure the title is displayed
    hwnd = FindWindow(NULL, uniq_title);
    SetConsoleTitle(save_title);
    
    return hwnd;
}

/* Try to determine if launched from a command-line (if yes do not hide
 * the console) or as a separate screen (hide the console at start).
 * If cursor in 0,0 launched from a separate screen. */

static int
Launched_From_Command_Line()
{
    HANDLE h_stdout;
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    int from_cmd_line;
    
    h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h_stdout == INVALID_HANDLE_VALUE)
        return TRUE;
    
    GetConsoleScreenBufferInfo(h_stdout, &csbi);
    from_cmd_line = (csbi.dwCursorPosition.X != 0 || csbi.dwCursorPosition.Y != 0);
    if (csbi.dwSize.X <= 0 || csbi.dwSize.Y <= 0)
        from_cmd_line = 1;
    
    return from_cmd_line;
}

static void
Show_Text_Console(int show_console)
{
    if (show_console)
    {
        ShowWindow(hwnd_console, SW_SHOW);   
        CheckMenuItem(GetMenu(hwndMain), IDM_SHOW_CONSOLE,
            MF_BYCOMMAND | MF_CHECKED);
    }
    else
    {
        ShowWindow(hwnd_console, SW_HIDE);   
        CheckMenuItem(GetMenu(hwndMain), IDM_SHOW_CONSOLE,
            MF_BYCOMMAND | MF_UNCHECKED);
    }
}
