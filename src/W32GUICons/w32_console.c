/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Win32 GUI console                                               *
 * File  : w32_console.c                                                   *
 * Descr.: W32 GUI Console                                                 *
 * Author: Jacob Navia and Daniel Diaz                                     *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/


#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <malloc.h>

#include "../EnginePl/gp_config.h" /* only to know the value of WITH_HTMLHELP */
#include "../EnginePl/set_locale.h"

#include "w32gc_interf.h"          /* only to know Query_Stack() cmd constants */
#include "../TopComp/prolog_path.c"

#define GUI_VERSION   "1.1"

#define ADDITIONAL_INFORMATION                    \
  "Windows GUI Console version " GUI_VERSION "\n" \
  "By Jacob Navia and Daniel Diaz\n\n"
#include "../TopComp/copying.c"

#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <richedit.h>
#include <shlobj.h>
#include <shellapi.h>

#include "w32_resource.h"


#ifdef _MSC_VER
#define _STATIC_CPPLIB
#endif

#ifdef WITH_HTMLHELP

/* HtmlHelp is used to display the doc (.chm file)
 *
 * 1) HtmlHelp can be statically linked (needs htmlhelp.lib or libhtmlhelp.a)
 * Recent versions of the lib are compiled with security options and needs
 * external check functions. The link errors are:
 *
 * libhtmlhelp.a: undefined reference to `__GSHandlerCheck'
 * libhtmlhelp.a: undefined reference to `__security_check_cookie'
 *
 * __GSHandlerCheck: due to the use MSVC /GS option (enable security check)
 * Some people solved this linking with a MSVC gshandler.obj but I could not
 * find it.
 *
 * __security_check_cookie: see http://support.microsoft.com/kb/894573
 * However, I could never find a valide bufferoverflowu.lib.
 * The solution consists in a set of fake (dummy) functions.
 *
 * 2) HtmlHelp can be dynamically loaded. This avoid the link with the lib
 * but needs hhctrl.ocx at runtime.
 */

#ifdef __GNUC__ /* ignore MSVC extensions present in htmlhelp.h */
#  define __in
#  define __out
#  define __in_opt
#endif

#include <htmlhelp.h>

#if WITH_HTMLHELP == 1 && defined(__GNUC__)
void __fastcall __GSHandlerCheck() {}
void __fastcall __security_check_cookie(unsigned* p) {}
unsigned* __security_cookie;
#endif

#endif  /* !WITH_HTMLHELP */


#if 1
#define DLLEXPORT __declspec(dllexport)
#endif

#if 0
#define DEBUG
#endif




/* xxPtr versions should exist now for both 32/64 bits - but in case of... */
#ifndef GetWindowLongPtr
#define GetWindowLongPtr GetWindowLong
#define SetWindowLongPtr SetWindowLong
#endif


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define EDIT_FIELD_SIZE            64000  /* 0 does not work ! why ? */

#define FIX_TAB                    1    // replace \t by ESC+tab
#define FIX_CR                     2    // remove \r
#define FIX_BACKSLASH              4    // replace \ by /
#define FIX_QUOTE                  8    // replace ' by ''




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/


/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

static char *(*fct_get_separators)();
static int (*fct_get_prompt_length)();
static PlLong (*fct_query_stack)();

static unsigned int queue[EDIT_FIELD_SIZE];
static int queue_start, queue_end;
static CRITICAL_SECTION cs_queue;
static HANDLE event_window_is_ready;
static HANDLE event_char_in_queue;

#define Queue_Is_Empty() (queue_start == queue_end)

#define Enqueue(c)                                      \
  do                                                    \
    {                                                   \
      queue[queue_end] = c;                             \
      queue_end = (queue_end + 1) % sizeof(queue);      \
      if (queue_end == queue_start)                     \
        queue_start = (queue_start + 1) % sizeof(queue);\
    }                                                   \
  while(0)


#define Dequeue(c)                                      \
  do                                                    \
    {                                                   \
      c = queue[queue_start];                           \
      queue_start = (queue_start + 1) % sizeof(queue);  \
    }                                                   \
  while(0)





static HWND hwndMain;           // Main window handle (same as hwnd in most fct)
static HWND hwndEditControl;    // Edit Control handle
static WNDPROC lpEProc;
static HINSTANCE hInst;         // Instance handle
static LOGFONT currentFont;     // Used font
static HFONT hFont;

static int show_console = 0;    // is the associated text console shown ?
static HWND hwnd_console;

static int in_get_char = 0;     // inside a Get_Char() ?
static int last_is_read = 0;    // to know if a msg box should be displayed at exit

static int posit = 0;           // position inside current (last) line
static int ec_start = 0;        // position of the begin in the Win edit control (corresponds to posit = 0)

static int win_x = CW_USEDEFAULT; // main window pos and size
static int win_y = CW_USEDEFAULT;
static int win_width = CW_USEDEFAULT;
static int win_height = CW_USEDEFAULT;

static int copy_on_sel = 1;     // default: automatically copy the selection
static int wrap_mode = 0;       // default: no word wrapping (line break if line > width)
static int line_buffering = 1;  // default: line buffered
static int beep_on_error = 0;   // default: no beep
static char wr_buffer[10240];   // when full a flush occurs (size does not matter)
static char *wr_buffer_ptr = wr_buffer;
static int dont_use_selection;  // is selection reliable (no if used to move the caret)


static char buff_pathname[MAX_PATH];




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

static int CallMain(void *unused);

static int StartWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                        LPSTR lpCmdLine, INT nCmdShow);

static BOOL InitApplication(void);

static HWND Createw32_consoleWndClassWnd(void);

static LRESULT CALLBACK MainWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

static void MainWndProc_OnCommand(HWND hwnd, int id, HWND hwndCtl, UINT codeNotify);


static void Create_Edit_Control(HWND hwnd);


static void SubClassEditField(HWND hwnd);

static LRESULT CALLBACK SubClassEdit(HWND hwnd, UINT msg, WPARAM mp1, LPARAM mp2);
static int CALLBACK WordBreakProc(LPTSTR lpcb, int ichCurrent, int cch, int code);

static void Toggle_Wrap_Mode(HWND hwnd);



static HFONT Create_Courier_Font(void);

static int Change_Font(HWND hwnd);



BOOL CALLBACK StackSizesProc(HWND hwndDlg, UINT message, WPARAM wParam, LPARAM lParam);




static void Load_Options(void);

static void Save_Options(void);

static void Activate_Options(void);


static char *Get_Current_Word(int select_it);

static void Consult_File(void);

static void Change_Directory(void);

static void Insert_File_Name(void);

static char *Get_Selected_File_Name(char *title, char *default_ext, char *filter);

static char *Get_Selected_Directory(char *title, int new_folder);

static void Show_Help(char *word);

static int Get_CHM_Help_Path(char *path);


static int WINAPI BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData);



static void Add_Clipboard_To_Queue(void);

static void Add_String_To_Queue(char *str, int mask_fix);

static void Add_Char_To_Queue(int c);

static void Set_Selection(int posit, int n);

static void Set_Caret_Position(int posit);

static int Move_Caret_To(int start_or_end);

static void Move_Caret_From_Mouse(int if_no_selection);

static int Delete_Selection(void);

static void Display_Text(char *str, int n);

static void Flush_Buffer(void);

#ifdef DEBUG
static int Console_Printf(char *format, ...);
#endif

DLLEXPORT void W32GC_Set_Line_Buffering(int is_buffered);
DLLEXPORT void W32GC_Backd(int n);
DLLEXPORT int W32GC_Confirm_Box(char *titre, char *msg);


static BOOL Launched_From_Command_Line();
static HWND Find_Text_Console_Handle(void);
static void Show_Text_Console(int show);

#define SET_CHECKED_OPT(idm_cmd, var)  \
  CheckMenuItem(GetMenu(hwndMain), idm_cmd, (var) ? MF_CHECKED : MF_UNCHECKED)


/* from terminal.h */

#define KEY_CTRL(x)                ((x) & 0x1f)
#define KEY_ESC(x)                 ((2<<8) | ((x)|0x20))




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


/* get_separators can be NULL (default separators are used).
 *
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
W32GC_Start_Window(char *(*get_separators)(), int (*get_prompt_length)(),
                   PlLong (*query_stack)())
{
  DWORD tid;

  Load_Options();

  fct_get_separators = get_separators;
  fct_get_prompt_length = get_prompt_length;
  fct_query_stack = query_stack;

  hwnd_console = Find_Text_Console_Handle();
  show_console |= Launched_From_Command_Line(); /* from command-line keep the console ! */

  InitializeCriticalSection(&cs_queue);
  event_window_is_ready = CreateEvent(NULL, FALSE, FALSE, NULL);

  CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE) CallMain, hwndMain, 0, &tid);

  // wait until windows manager ok in the thread
  WaitForSingleObject(event_window_is_ready, INFINITE);
  CloseHandle(event_window_is_ready);
  return 1;
}


static int
CallMain(void *unused)
{
  StartWinMain(hInst, 0, "", SW_SHOW);
  return 0;
}

static int
StartWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, INT nCmdShow)
{
  MSG msg;
  HACCEL hAccelTable;

  /* Reinit the locale in the DLL in case it has its own CRT
   * (or else compile with MSVC option -MD)
   */

  Set_Locale();

  hInst = hInstance;

  event_char_in_queue = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (!InitApplication())
    return 0;
  hAccelTable = LoadAccelerators(hInst, MAKEINTRESOURCE(IDR_ACCEL));
  if ((hwndMain = Createw32_consoleWndClassWnd()) == (HWND) 0)
    return 0;
  ShowWindow(hwndMain, SW_SHOW);

  Activate_Options();
  if (fct_query_stack == NULL)
    EnableMenuItem(GetMenu(hwndMain), IDM_STACK_SIZES, MF_BYCOMMAND | MF_GRAYED);

  while (GetMessage(&msg, NULL, 0, 0))
    {
      if (!TranslateAccelerator(hwndMain, hAccelTable, &msg))
        {
          TranslateMessage(&msg);
          DispatchMessage(&msg);
        }
    }
  CloseHandle(event_char_in_queue);
  DeleteCriticalSection(&cs_queue);
  DestroyAcceleratorTable(hAccelTable);

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
  //  wc.hIcon = LoadIcon(NULL, IDI_WINLOGO);
  wc.hIcon = LoadIcon(hInst, MAKEINTRESOURCE(IDI_ICON));
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
                      WS_BORDER | WS_SYSMENU | WS_THICKFRAME,
                      win_x, win_y, win_width, win_height,
                      NULL, NULL, hInst, NULL);
}



static LRESULT CALLBACK
MainWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  PAINTSTRUCT ps;

  switch (msg)
    {
    case WM_CREATE:
      Create_Edit_Control(hwnd);
      SetEvent(event_window_is_ready);
      return 0; /* 0 means message has been treated */

    case WM_SIZE:
      MoveWindow(hwndEditControl, 0, 0, LOWORD(lParam), HIWORD(lParam), TRUE);
      SendMessage(hwndEditControl, EM_SCROLLCARET, 0, 0); // be sure the caret is visble
      return 0;

    case WM_SETFOCUS:
      SetFocus(hwndEditControl);
      return 0;

    case WM_COMMAND:
      HANDLE_WM_COMMAND(hwnd, wParam, lParam, MainWndProc_OnCommand);
      return 0;
#if 0
    case WM_CLOSE:
      if (!W32GC_Confirm_Box("GNU Prolog", "Really quit ?"))
        return 0;
      break;
#endif
    case WM_PAINT:
      BeginPaint(hwnd, &ps);
      EndPaint(hwnd, &ps);
      return 0;

    case WM_DESTROY:
      PostQuitMessage(0);
      exit(0);
      return 0;
    }

  return DefWindowProc(hwnd, msg, wParam, lParam);
}



static void
MainWndProc_OnCommand(HWND hwnd, int id, HWND hwndCtl, UINT codeNotify)
{
  switch (id)
    {
    case IDM_CONSULT:
      Consult_File();
      break;

    case IDM_CHDIR:
      Change_Directory();
      break;

    case IDM_FILE_NAME:
      Insert_File_Name();
      break;

    case IDM_EXIT:
      PostMessage(hwnd, WM_CLOSE, 0, 0);
      break;


    case IDM_COPY:
      SendMessage(hwndEditControl, WM_COPY, 0, 0);
      break;

    case IDM_PASTE:
      Add_Clipboard_To_Queue();
      break;

    case IDM_SELECT_ALL:
      SendMessage(hwndEditControl, EM_SETSEL, 0, -1);
      if (copy_on_sel)
        SendMessage(hwndEditControl, WM_COPY, 0, 0);
      break;

    case IDM_SAVE_OPTIONS:
      Save_Options();
      break;

    case IDM_COPY_ON_SEL:
      copy_on_sel = 1 - copy_on_sel;
      SET_CHECKED_OPT(IDM_COPY_ON_SEL, copy_on_sel);
      break;


    case IDM_INTERRUPT:
      if (in_get_char)
        Add_Char_To_Queue(KEY_CTRL('C'));
      else
        GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
      break;

    case IDM_STACK_SIZES:
      /* DialogBox() returns IDOK if validated by button OK (else aborted by button CANCEL) */
      DialogBox(hInst, MAKEINTRESOURCE(IDD_STACK_SIZES), hwndMain, (DLGPROC) StackSizesProc);
      break;

    case IDM_WRAP:
      Toggle_Wrap_Mode(hwnd);
      break;

    case IDM_BEEP:
      beep_on_error = 1 - beep_on_error;
      SET_CHECKED_OPT(IDM_BEEP, beep_on_error);
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
      Change_Font(hwnd);
      break;

    case IDM_MANUAL:
      Show_Help(NULL);
      break;

    case IDM_INDEX:
      Show_Help(Get_Current_Word(1));
      break;

    case IDM_WEB:
      ShellExecute(NULL, "open", "http://www.gprolog.org/", NULL, ".", 0);
      break;

    case IDM_ABOUT:
      MessageBox(hwndMain, Mk_Copying_Message(NULL), "About GNU Prolog", MB_OK | MB_ICONINFORMATION);
      break;
    }
}

/*<---------------------------------------------------------------------->*/

static void
Create_Edit_Control(HWND hwnd)
{
  RECT rc;

  GetClientRect(hwnd, &rc);
  hwndEditControl = CreateWindow("EDIT",
                                 NULL,
                                 WS_CHILD | WS_VISIBLE | ES_MULTILINE |
                                 WS_VSCROLL | ES_AUTOVSCROLL |
                                 (wrap_mode ? 0 : (WS_HSCROLL | ES_AUTOHSCROLL)) |
                                 ES_NOHIDESEL,
                                 0,
                                 0,
                                 (rc.right - rc.left),
                                 (rc.bottom - rc.top),
                                 hwnd, (HMENU) 1000, hInst, NULL);
  SetWindowLongPtr(hwnd, DWLP_USER, (LONG_PTR) hwndEditControl);
  SendMessage(hwndEditControl, WM_SETFONT, (WPARAM) hFont, 0L);
  SubClassEditField(hwndEditControl);
  SendMessage(hwndEditControl, EM_SETLIMITTEXT, EDIT_FIELD_SIZE, 0);
  if (wrap_mode)
    SendMessage(hwndEditControl, EM_SETWORDBREAKPROC, (WPARAM) 0, (LPARAM) WordBreakProc);
  SetFocus(hwndEditControl);
}


static void
SubClassEditField(HWND hwnd)
{
  if (lpEProc == NULL)
    lpEProc = (WNDPROC) GetWindowLongPtr(hwnd, GWLP_WNDPROC);
  SetWindowLongPtr(hwnd, GWLP_WNDPROC, (LONG_PTR) SubClassEdit);
}

static LRESULT CALLBACK
SubClassEdit(HWND hwnd, UINT msg, WPARAM mp1, LPARAM mp2)
{
  LRESULT r;
  int c, del, repeat, hasCtrl, hasAlt;
  unsigned char pKeyState[256];

  GetKeyboardState(pKeyState);
  hasCtrl = (pKeyState[VK_CONTROL] & (unsigned char) 0x80);
  hasAlt = (pKeyState[VK_MENU] & (unsigned char) 0x80);

  if (msg == WM_CHAR)
    {
      repeat = (int) (mp2 & 0xffff);
      if (hasCtrl && !hasAlt) // only needed for ^space (for ^A mp1 is already 1, but not for ^space)
        mp1 = KEY_CTRL(mp1);  // we test !hasAlt because AltGr is the same as Ctrl+Alt

      del = (mp1 == '\b' || mp1 == KEY_CTRL('D') || isprint(mp1)) ? Delete_Selection() : 0;

      if (del && (mp1 == '\b' || mp1 == KEY_CTRL('D')) && --repeat == 0)
        return 0;

      while (repeat--)
        Add_Char_To_Queue(mp1);

      return 0;
    }

  if (msg == WM_KEYDOWN)
    {
#if 0                           /* now done by accelerators defined in resources */
      if ((mp1 == 'c' || mp1 == 'C') && hasCtrl)
        {
          SendMessage(hwndMain, WM_COMMAND, IDM_COPY, 0);
          return 0;
        }
      if ((mp1 == 'v' || mp1 == 'V') && hasCtrl)
        {
          SendMessage(hwndMain, WM_COMMAND, IDM_PASTE, 0);
          return 0;
        }
#endif
      c = 0;
      switch (mp1)
        {
        case VK_NEXT:           /* default vertical scroll behavior */
        case VK_PRIOR:
          break;

        case VK_DELETE:
          if (Delete_Selection())
            break;    /* else like other keys */
        case VK_LEFT:
        case VK_RIGHT:
        case VK_UP:
        case VK_DOWN:
        case VK_HOME:
        case VK_END:
        case VK_INSERT:
          Move_Caret_From_Mouse(0);
          c = (hasCtrl) ? 2 : 1;
          c = ((c << 8) | mp1);
          Add_Char_To_Queue(c);
          return 0;
#if 0
        case VK_F1:             /* now done by an accelerator */
          Show_Help(Get_Current_Word(1));
          return 0;
#endif

#ifdef DEBUG /* to include a test code */
        case VK_F2:
          {
            int size = SendMessage(hwndEditControl, EM_GETLIMITTEXT, 0, 0);
            int len = SendMessage(hwndEditControl, WM_GETTEXTLENGTH, 0, 0);
            char s[100];

            int beg, end;
            Set_Selection(3, 200);
            SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &beg, (WPARAM) &end);

            sprintf(s,"limit: %d   len: %d   sel: %d-%d", size, len, beg, end);
            MessageBox(NULL, s, "Error", MB_OK);
            // size += 10;
            SendMessage(hwndEditControl, EM_SETLIMITTEXT, size, 0);
            return 0;
          }

        case VK_F3:
          {
            char s[100];
            Save_Options();
            sprintf(s, "cur word: <%s>", Get_Current_Word(1));
            MessageBox(NULL, s, "Error", MB_OK);
            return 0;
          }

        case VK_F4:
          {
          }
          return 0;
#endif
        }
    }

  if (msg == WM_RBUTTONUP)      /* deactivate right buttom (replace by paste) */
    {
      Add_Clipboard_To_Queue();
      return 0;
    }

  if (msg == WM_MBUTTONUP)      /* middle buttom = paste*/
    {
      Add_Clipboard_To_Queue();
      return 0;
    }

  if (msg == WM_LBUTTONDBLCLK)  /* double-click: select word */
    {
      //  r = CallWindowProc(lpEProc, hwnd, msg, mp1, mp2);
      Get_Current_Word(1);
      return 0;
    }

  /* default behavior */

  r = CallWindowProc(lpEProc, hwnd, msg, mp1, mp2);

  if (msg == WM_LBUTTONUP)      /* left button (inside cur line) move the caret */
    Move_Caret_From_Mouse(1);

  return r;
}


/* This WordBreakProc is to avoid word wrapping
 * (we simply want "char wrapping": a line break occurs when the line is full)
 * just return 0
 */
static int CALLBACK
WordBreakProc(LPTSTR lpcb, int ichCurrent, int cch, int code)
{
  return 0;
}


static void
Toggle_Wrap_Mode(HWND hwnd)
{
  int start, end;
  int text_size;
  char *text;

  wrap_mode = 1 - wrap_mode;
  SET_CHECKED_OPT(IDM_WRAP, wrap_mode);

  /* Destroy and recreate the edit control window. For no wrapping pass the options
   * AUTOHSCROLL | HSCROLL (for wrapping do not pass them).
   */

  LockWindowUpdate(hwnd);       /* lock to avoid flicking */

  SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &start, (LPARAM) &end); // save selection
  text_size = SendMessage(hwndEditControl, WM_GETTEXTLENGTH, 0, 0) + 1; // + 1 for '\0'
  text = malloc(text_size); // for the '\0'
  if (text != NULL)
    {
      SendMessage(hwndEditControl, WM_GETTEXT, (WPARAM) text_size, (LPARAM) text); /* save the text */

      ShowWindow(hwndEditControl, SW_HIDE); /* hide and destroy current edit control */
      DestroyWindow(hwndEditControl);

      Create_Edit_Control(hwnd);    /* re-create the edit control and show it */
      ShowWindow(hwndEditControl, SW_SHOW);

      SendMessage(hwndEditControl, WM_SETTEXT, (WPARAM) 0, (LPARAM) text); /* re-copy saved text */
      SendMessage(hwndEditControl, EM_SETSEL, (WPARAM) start, (LPARAM) end); // restore selection
      SendMessage(hwndEditControl, EM_SCROLLCARET, 0, 0); // be sure the caret is visble

      free(text);
    }
  LockWindowUpdate(NULL);       /* unlock the window */
}


/*<---------------------------------------------------------------------->*/

static HFONT
Create_Courier_Font(void)
{
  memset(&currentFont, 0, sizeof(LOGFONT));

  currentFont.lfCharSet = ANSI_CHARSET;
  currentFont.lfWeight = FW_NORMAL;
  currentFont.lfHeight = 18;
  currentFont.lfPitchAndFamily = (BYTE) (FIXED_PITCH | FF_MODERN);
  strcpy(currentFont.lfFaceName, "Courier");    /* Courier */
  return CreateFontIndirect(&currentFont);
}

static int
Change_Font(HWND hwnd)
{
  LOGFONT lf;
  CHOOSEFONT cf;

  memset(&cf, 0, sizeof(CHOOSEFONT));
  memcpy(&lf, &currentFont, sizeof(LOGFONT));
  cf.lStructSize = sizeof(CHOOSEFONT);

  cf.hwndOwner = hwnd;
  cf.lpLogFont = &lf;
  cf.Flags = CF_SCREENFONTS | CF_FIXEDPITCHONLY | CF_INITTOLOGFONTSTRUCT;
  cf.nFontType = SCREEN_FONTTYPE;

  if (!ChooseFont(&cf))
    return 0;

  memcpy(&currentFont, &lf, sizeof(LOGFONT));

  hFont = CreateFontIndirect(&currentFont);
  SendMessage(hwndEditControl, WM_SETFONT, (WPARAM) hFont, 0);
  InvalidateRect(hwndEditControl, NULL, 1);
  SendMessage(hwndEditControl, EM_SCROLLCARET, 0, 0);
  return 1;
}

/*<---------------------------------------------------------------------->*/

struct {
  int idc_desc;
  int idc_def_sz;
  int idc_env_var_name;
  int idc_env_var_sz;
  int idc_reg_sz;
  int idc_cur_sz;
} stk[] = {
  { IDC_STACK_DESC0, IDC_DEF_SZ0, IDC_ENV_VAR_NAME0, IDC_ENV_SZ0, IDC_REG_SZ0, IDC_CUR_SZ0 },
  { IDC_STACK_DESC1, IDC_DEF_SZ1, IDC_ENV_VAR_NAME1, IDC_ENV_SZ1, IDC_REG_SZ1, IDC_CUR_SZ1 },
  { IDC_STACK_DESC2, IDC_DEF_SZ2, IDC_ENV_VAR_NAME2, IDC_ENV_SZ2, IDC_REG_SZ2, IDC_CUR_SZ2 },
  { IDC_STACK_DESC3, IDC_DEF_SZ3, IDC_ENV_VAR_NAME3, IDC_ENV_SZ3, IDC_REG_SZ3, IDC_CUR_SZ3 },
  /* for max_atom */
  { IDC_STACK_DESC4, IDC_DEF_SZ4, IDC_ENV_VAR_NAME4, IDC_ENV_SZ4, IDC_REG_SZ4, IDC_CUR_SZ4 }
};


BOOL CALLBACK
StackSizesProc(HWND hwndDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
  int nb_stk, i;
  char *desc, *env_var_name, *p;
  int def_sz, cur_sz;
  DWORD reg_sz;
  BOOL ok;

  switch (message)
    {
    case WM_INITDIALOG:
      if ((*fct_query_stack)(QUERY_STACK_HAS_FIXED_SIZES, 0))
        {
          SetDlgItemText(hwndDlg, IDC_FIXED_SIZES,
                         "This application is compiled with fixed stack sizes, "
                         "it ignores customized stack sizes.");
#if 0
          // change style: border around
          HWND hwndFixed = GetDlgItem(hwndDlg, IDC_FIXED_SIZES);
          LONG_PTR style = GetWindowLongPtr(hwndFixed, GWL_STYLE);
          SetWindowLongPtr(hwndFixed, GWL_STYLE, style | WS_BORDER);

          /* NB: after changing a style it is necessary to call SetWindowPos
           * to alert Windows that the window has changed (Humm)
           */
          SetWindowPos(hwndFixed, NULL, 0, 0, 0, 0,
                       SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED);
#endif
        }
      else
        {
          SetDlgItemText(hwndDlg, IDC_FIXED_SIZES,
                         "New sizes will be taken into account at the next restart.");
        }


      nb_stk = (*fct_query_stack)(QUERY_STACK_GET_NB_OF_STACKS, 0);

      for (i = 0; i < nb_stk; i++)
        {
          desc = (char *) (*fct_query_stack)(QUERY_STACK_GET_DESC, i);
          env_var_name = (char *) (*fct_query_stack)(QUERY_STACK_GET_ENV_VAR_NAME, i);
          def_sz = (*fct_query_stack)(QUERY_STACK_GET_DEFAULT_SIZE, i);
          cur_sz = (*fct_query_stack)(QUERY_STACK_GET_SIZE, i);

          SetDlgItemText(hwndDlg, stk[i].idc_desc, desc);
          SetDlgItemInt(hwndDlg, stk[i].idc_def_sz, def_sz, FALSE);
          SetDlgItemText(hwndDlg, stk[i].idc_env_var_name, env_var_name);
          if ((p = getenv(env_var_name)) != NULL)
            SetDlgItemText(hwndDlg, stk[i].idc_env_var_sz, p);

          if (Read_Windows_Registry(env_var_name, REG_DWORD, &reg_sz, sizeof(reg_sz)))
            SetDlgItemInt(hwndDlg, stk[i].idc_reg_sz, reg_sz, FALSE);

          SetDlgItemInt(hwndDlg, stk[i].idc_cur_sz, cur_sz, FALSE);
        }
      return 0;

    case WM_COMMAND:
      switch (LOWORD(wParam))
        {
        case IDOK:
          nb_stk = (*fct_query_stack)(QUERY_STACK_GET_NB_OF_STACKS, 0);
          for (i = 0; i < nb_stk; i++)
            {
              env_var_name = (char *) (*fct_query_stack)(QUERY_STACK_GET_ENV_VAR_NAME, i);
              reg_sz = GetDlgItemInt(hwndDlg, stk[i].idc_reg_sz, &ok, FALSE);
              if (reg_sz > 0 && ok) /* in fact if !ok returned value = 0 (could pass NULL for ok) */
                Write_Windows_Registry(env_var_name, REG_DWORD, &reg_sz, sizeof(reg_sz));
              else
                Delete_Windows_Registry(env_var_name);
            }
          // then in IDCANCEL to also execute EndDialog()
        case IDCANCEL:
          EndDialog(hwndDlg, wParam);
          return TRUE;
        }
    }
  return FALSE;
}

/*<---------------------------------------------------------------------->*/

#define READ_INT_REG(key_name, var)                             \
{                                                               \
  DWORD x;                                                      \
  if (Read_Windows_Registry(key_name, REG_DWORD, &x, sizeof(x)))\
    var = x;                                                    \
}

#define WRITE_INT_REG(key_name, var)                            \
{                                                               \
  DWORD x = (DWORD) var;                                        \
  Write_Windows_Registry(key_name, REG_DWORD, &x, sizeof(x));   \
}


#define READ_BOOL_REG(key_name, var)                            \
{                                                               \
  unsigned char x;                                              \
  if (Read_Windows_Registry(key_name, REG_BINARY, &x, 1))       \
    var = x;                                                    \
}

#define WRITE_BOOL_REG(key_name, var)                           \
{                                                               \
  unsigned char x = (unsigned char) var;                        \
  Write_Windows_Registry(key_name, REG_BINARY, &x, 1);          \
}


static void
Save_Options(void)
{
  WINDOWPLACEMENT wndpl;

  WRITE_BOOL_REG("GUIAutoCopyOnSel", copy_on_sel);
  WRITE_BOOL_REG("GUIBeepOnError", beep_on_error);
  WRITE_BOOL_REG("GUIWrapMode", wrap_mode);
  WRITE_BOOL_REG("GUILineBuffering", line_buffering);
  WRITE_BOOL_REG("GUIShowTextConsole", show_console);

  if (GetWindowPlacement(hwndMain, &wndpl))
    {
      RECT rc = wndpl.rcNormalPosition;
      win_x = rc.left;
      win_y = rc.top;
      win_width = rc.right - rc.left;
      win_height = rc.bottom - rc.top;
      //printf("CURR  %d / %d   %d x %d\n", win_x, win_y, win_width, win_height);
      WRITE_INT_REG("GUIPosX", win_x);
      WRITE_INT_REG("GUIPosY", win_y);
      WRITE_INT_REG("GUIWidth", win_width);
      WRITE_INT_REG("GUIHeight", win_height);
    }

  Write_Windows_Registry("GUIFont", REG_BINARY, &currentFont, sizeof(currentFont));
}

static void
Load_Options(void)
{
  LOGFONT lf;

  READ_INT_REG("GUIPosX", win_x);
  READ_INT_REG("GUIPosY", win_y);
  READ_INT_REG("GUIWidth", win_width);
  READ_INT_REG("GUIHeight", win_height);

  READ_BOOL_REG("GUIAutoCopyOnSel", copy_on_sel);
  READ_BOOL_REG("GUIBeepOnError", beep_on_error);
  READ_BOOL_REG("GUIWrapMode", wrap_mode);
  READ_BOOL_REG("GUILineBuffering", line_buffering);
  READ_BOOL_REG("GUIShowTextConsole", show_console);

  if (Read_Windows_Registry("GUIFont", REG_BINARY, &lf, sizeof(lf)))
    {
      currentFont = lf;
      hFont = CreateFontIndirect(&currentFont);
    }
  else
    hFont = Create_Courier_Font(); /* default font */
}


static void
Activate_Options(void)
{
  SET_CHECKED_OPT(IDM_COPY_ON_SEL, copy_on_sel);
  SET_CHECKED_OPT(IDM_BEEP, beep_on_error);
  SET_CHECKED_OPT(IDM_WRAP, wrap_mode);
  SET_CHECKED_OPT(IDM_BUFFERING, line_buffering);

  W32GC_Set_Line_Buffering(line_buffering);

  Show_Text_Console(show_console);
}



/*<---------------------------------------------------------------------->*/

#define Is_A_Sep(c)  (!isprint(c) || (strchr(separators, c) != NULL))

static char *
Get_Current_Word(int select_it)
{
  static char *text = NULL;
  int text_size;
  int start, end;
  char *p, *q;
  // default separators must not be static (else the default value can be changed)
  char *separators = " ,;:-'\"!@$#^&()-+*/\\[]|<=>`~{}";

  if (text != NULL)
    free(text);

  text_size = SendMessage(hwndEditControl, WM_GETTEXTLENGTH, 0, 0) + 1; // + 1 for '\0'
  text = malloc(text_size); // for the '\0'
  if (text == NULL)
    return "";

  SendMessage(hwndEditControl, WM_GETTEXT, (WPARAM) text_size, (LPARAM) text); /* save the text */
  SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &start, (LPARAM) &end);

  if (fct_get_separators)
    separators = (*fct_get_separators)();

  p = q = text + start;
  if (!Is_A_Sep(*p))            /* else an empty word */
    {
      while (p >= text && !Is_A_Sep(*p))
        p--;
      p++;
      while (*q && !Is_A_Sep(*q))
        q++;
    }
  *q = '\0';

  if (select_it)
    {
      SendMessage(hwndEditControl, EM_SETSEL, (WPARAM) (p - text), (LPARAM) (q - text));
      if (copy_on_sel)
        SendMessage(hwndEditControl, WM_COPY, 0, 0);
    }

  return p;
}


static void
Consult_File(void)
{
  char *p = Get_Selected_File_Name("Consult...", "pl",
                                   "Prolog Files (.pl .pro),*.pl;*.pro,All Files,*.*");

  if (p == NULL)
    return;

  Add_Char_To_Queue(KEY_CTRL('A'));
  Add_Char_To_Queue(KEY_CTRL('K'));
  Add_String_To_Queue("consult('", 0);
  Add_String_To_Queue(p, FIX_TAB | FIX_CR | FIX_BACKSLASH | FIX_QUOTE);
  Add_String_To_Queue("').\n", 0);
}


static void
Change_Directory(void)
{
  char *p = Get_Selected_Directory("Select working directory", 1);

  if (p == NULL)
    return;

  Add_Char_To_Queue(KEY_CTRL('A'));
  Add_Char_To_Queue(KEY_CTRL('K'));
  Add_String_To_Queue("change_directory('", 0);
  Add_String_To_Queue(p, FIX_TAB | FIX_CR | FIX_BACKSLASH | FIX_QUOTE);
  Add_String_To_Queue("').\n", 0);

#ifdef DEBUG
  SetCurrentDirectory(p);
#endif
}


static void
Insert_File_Name(void)
{
  char *p = Get_Selected_File_Name("Pick a file name...", NULL,
                                   "Prolog Files (.pl .pro),*.pl;*.pro,All Files,*.*");

  if (p == NULL)
    return;

  Add_Char_To_Queue('\'');
  Add_String_To_Queue(p, FIX_TAB | FIX_CR | FIX_BACKSLASH | FIX_QUOTE);
  Add_Char_To_Queue('\'');
}

static char *
Get_Selected_File_Name(char *title, char *default_ext, char *filter)
{
  char tmp_filter[128], *p;
  static char cwd[MAX_PATH];
  static char last_cwd[MAX_PATH];
  OPENFILENAME ofn;

  for(p = tmp_filter; *filter; filter++, p++) {
    *p = *filter;
    if (*p == ',')
      *p = '\0';
  }
  *p++ = '\0'; *p = '\0';

  if (GetCurrentDirectory(sizeof(cwd), cwd) != 0 && strcmp(cwd, last_cwd) != 0)
    strcpy(last_cwd, cwd);
  else
    strcpy(cwd, ".");

#if 0
  printf("USED DIR: %s\n", cwd);
#endif

  memset(&ofn,0,sizeof(ofn));
  ofn.lStructSize = sizeof(ofn);
  ofn.hwndOwner = GetActiveWindow();
  ofn.hInstance = GetModuleHandle(NULL);
  ofn.lpstrFilter = tmp_filter;
  ofn.nFilterIndex = 0;
  ofn.lpstrFile = buff_pathname;
  ofn.nMaxFile = sizeof(buff_pathname);
  ofn.lpstrTitle = title;
  ofn.lpstrDefExt = default_ext;
  ofn.lpstrInitialDir = cwd;
  *buff_pathname = '\0';
  ofn.Flags = OFN_EXPLORER | OFN_HIDEREADONLY | OFN_ENABLESIZING | OFN_PATHMUSTEXIST ;
  return GetOpenFileName(&ofn) ? buff_pathname : NULL;
}



#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE 0x40 // new style
#endif
#ifndef BIF_NONEWFOLDERBUTTON
#define BIF_NONEWFOLDERBUTTON 0x0200 // dont show "create new folder" button
#endif

static char *
Get_Selected_Directory(char *title, int new_folder)
{
  LPMALLOC pMalloc;
  BROWSEINFO browseInfo;
  LPITEMIDLIST lpItemIDList;

  CoInitialize(0);      // needed for BIF_NEWDIALOGSTYLE
  if (S_OK != SHGetMalloc(&pMalloc))
    return 0;

  memset(&browseInfo, 0, sizeof(BROWSEINFO));

  browseInfo.hwndOwner = GetActiveWindow();
  browseInfo.lpszTitle = title;
  browseInfo.lpfn = BrowseCallbackProc;
  browseInfo.ulFlags = BIF_NEWDIALOGSTYLE;
  if (!new_folder)
    browseInfo.ulFlags |= BIF_NONEWFOLDERBUTTON;
  lpItemIDList = SHBrowseForFolder(&browseInfo);
  *buff_pathname = '\0';
  if (lpItemIDList != NULL)
    {
      SHGetPathFromIDList(lpItemIDList, buff_pathname);
      pMalloc->lpVtbl->Free(pMalloc, lpItemIDList);
    }
  pMalloc->lpVtbl->Release(pMalloc);
  CoUninitialize();
  return (*buff_pathname) ? buff_pathname : NULL;
}



static int WINAPI
BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lp, LPARAM pData)
{
  switch (uMsg)
    {
    case BFFM_INITIALIZED:
      {
        if (GetCurrentDirectory(sizeof(buff_pathname), buff_pathname))
          {
            // WParam is TRUE since you are passing a path.
            // It would be FALSE if you were passing a pidl.
            SendMessage(hwnd, BFFM_SETSELECTION, TRUE, (LPARAM) buff_pathname);
          }
        break;
      }
    case BFFM_SELCHANGED:
      {
        // Set the status window to the currently selected path.
        if (SHGetPathFromIDList((LPITEMIDLIST) lp, buff_pathname))
          {
            SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, (LPARAM) buff_pathname);
          }
        break;
      }
    default:
      break;
    }
  return 0;
}

static void
Show_Help(char *word)
{
#ifdef WITH_HTMLHELP

  char help_path[1024];
  HWND hwnd = 0;                // or GetDesktopWindow();
  UINT command;
  HH_AKLINK link;
  DWORD_PTR data;

#if defined(WITH_HTMLHELP) && WITH_HTMLHELP == 2 /* load HtmlHelp dynamically */
  typedef HWND (WINAPI *FHH)();
  HINSTANCE inst;
  static FHH HtmlHelp;

  if (HtmlHelp == NULL &&
      ((inst = LoadLibrary("hhctrl.ocx")) == NULL ||
       (HtmlHelp = (FHH) GetProcAddress(inst, "HtmlHelpA")) == NULL))
    {
      MessageBox(NULL, "Error loading hhctrl.ocx / HtmlHelpA", "Error",
                 MB_OK);
      return;
    }
#endif
  if (!Get_CHM_Help_Path(help_path))    /* if CANCEL, abort */
    return;

  if (word == NULL)             /* open first page of the manual */
    {                           /* use strcat(help_path, "::/file.html#target") to open a specific page+target */
      command = HH_DISPLAY_TOPIC;
      data = 0;
    }
  else
    {
      link.cbStruct = sizeof(HH_AKLINK);

      link.fReserved = FALSE;
      link.pszKeywords = word;
      link.pszUrl = NULL;       // or .chm://index.html ?
      link.pszMsgText = NULL;
      link.pszMsgTitle = NULL;
      link.pszWindow = NULL;
      link.fIndexOnFail = TRUE;
      command = HH_KEYWORD_LOOKUP;
      data = (DWORD_PTR) &link;
    }

  if (HtmlHelp(hwnd, help_path, command, data) == 0)
    MessageBox(NULL, help_path, "HtmlHelp Error", MB_OK);

#else  /* !WITH_HTMLHELP */

  if (word == NULL)
    ShellExecute(NULL, "open", "http://gprolog.org/manual/html_node/index.html", NULL, ".", 0);
  else
    ShellExecute(NULL, "open", "http://gprolog.org/manual/html_node/gprolog-idx.html", NULL, ".", 0);

#endif /* !WITH_HTMLHELP */
}



static int
Get_CHM_Help_Path(char *path)
{
  char *p;
  int devel_mode;

  for (;;)
    {
      if ((p = Get_Prolog_Path(NULL, &devel_mode)) != NULL)
        break;

      if ((p = Get_Selected_Directory("Select the GNU Prolog directory", 0)) == NULL)
        return 0;

      Write_Windows_Registry("RootPath", REG_SZ, p, strlen(p));
    }

#ifdef DEBUG                    /* to force display + remove path (debug) */
  MessageBox(NULL, p, "Prolog Root Path", MB_OK);
  Write_Windows_Registry("RootPath", REG_SZ, p, strlen(p));
  //  Write_Windows_Registry("RootPath", REG_SZ, "", 0);
#endif

  if (devel_mode)
    {
      sprintf(path, "%s\\..\\doc\\gprolog.chm", p);
      if (access(path, F_OK) != 0)
        sprintf(path, "%s\\..\\..\\doc\\gprolog.chm", p);
    }
  else
    sprintf(path, "%s\\doc\\gprolog.chm", p);
  return 1;
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
          char *str = (char *) GlobalLock(hClipData);

          if (str)
            Add_String_To_Queue(str, FIX_TAB | FIX_CR);

          GlobalUnlock(hClipData);
        }
      CloseClipboard();
    }
}

static void
Add_String_To_Queue(char *str, int mask_fix)
{
  int c;

  EnterCriticalSection(&cs_queue);
  SetEvent(event_char_in_queue);
  while(*str)
    {
      c = *str++;
      if (c == '\r' && (mask_fix & FIX_CR))
        continue;

      if (c == '\t' && (mask_fix & FIX_TAB))
        c = KEY_ESC('\t');

      if (c == '\\' && (mask_fix & FIX_BACKSLASH))
        c = '/';

      if (c == '\'' && (mask_fix & FIX_QUOTE))
        Enqueue('\'');

      Enqueue(c);
    }
  LeaveCriticalSection(&cs_queue);
  SetEvent(event_char_in_queue);
}


static void
Add_Char_To_Queue(int c)
{
  EnterCriticalSection(&cs_queue);
  Enqueue(c);
  LeaveCriticalSection(&cs_queue);
  SetEvent(event_char_in_queue);
}


DLLEXPORT int
W32GC_Kbd_Is_Not_Empty()
{
  return !Queue_Is_Empty();
}

DLLEXPORT int
W32GC_Get_Char0()
{
  int result;

  in_get_char = 1;

  Flush_Buffer();               /* synchronize output and posit */

  last_is_read = 1;
  while (Queue_Is_Empty())
    {
      WaitForSingleObject(event_char_in_queue, INFINITE);
    }

  EnterCriticalSection(&cs_queue);
  Dequeue(result);
  LeaveCriticalSection(&cs_queue);

  in_get_char = 0;
  last_is_read = 1;

  return result;
}


static void
Set_Selection(int posit, int n)
{
  SendMessage(hwndEditControl, EM_SETSEL, ec_start + posit, ec_start + posit + n);
  SendMessage(hwndEditControl, EM_SCROLLCARET, 0, 0); // ensure the caret is visible
}

static void
Set_Caret_Position(int posit)
{
  Set_Selection(posit, 0);
}


static int
Move_Caret_To(int start_or_end)
{
  int prompt_length = (fct_get_prompt_length) ? (*fct_get_prompt_length)() : 0;
  int c = (1 << 8) | VK_RIGHT;
  int count;

  start_or_end -= ec_start;                 /* < 0 if not in the current (last) line */
  if (start_or_end < prompt_length)     /* not in cur line or in the prompt: do nothing */
    {
      //      Set_Selection(posit, 0); // to prevent the move of the caret outside last line - comment if needed (commented because do not work well with double-click = word selection)
      return 0;
    }
  count = start_or_end - posit;
  if (count < 0)
    {
      count = -count;
      c = (1 << 8) | VK_LEFT;
    }
  while (count--)
    Add_Char_To_Queue(c);

  return 1;
}


static void
Move_Caret_From_Mouse(int if_no_selection)
{
  int start, end;

  SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &start, (LPARAM) &end);
  if (start != end)
    {
      if (copy_on_sel)
        SendMessage(hwndEditControl, WM_COPY, 0, 0);
      if (if_no_selection)
        return;
    }

  Move_Caret_To(start);
}


static int
Delete_Selection(void)
{
  int start, end;
  int count;

  if (dont_use_selection)       /* is the selection reliable ? */
    return 0;

  SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &start, (LPARAM) &end);

  count = end - start;

  if (count == 0 || !Move_Caret_To(start))
    return 0;

  while (count--)
    Add_Char_To_Queue(KEY_CTRL('D'));

  return 1;
}

static void
Display_Text(char *str, int n)
{
  int end;
  while (n--)
    {
      switch (*str)
        {
        case '\r':
          break;

        case '\n':
          Flush_Buffer();       /* emit the line */
          dont_use_selection = 1;
          SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) "\r\n");
          SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &ec_start, (WPARAM) &end);
          dont_use_selection = 0;
          posit = 0;            /* A NEW LINE begins here */
          break;

        case '\b':              /* really needed only if W32GC_Backd is not defined */
          W32GC_Backd(1);       /* else simply call it ! */
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
  int n;
  int max_size, text_size;
  int end;


  n = wr_buffer_ptr - wr_buffer;
  if (n > 0)
    {
      /* the n chararacters have to be written from ec_start + posit to ec_start + posit + n - 1
       * it is important to replacesel (not to insert) because linedit will rewrite all next
       * chars in insert mode. Should be viewed as always in overwrite mode.
       *
       * Since we use the selection to replace chars, the selection should not be taken into
       * account by other threads. This is the reason we protect the code with dont_use_selection
       */

      dont_use_selection = 1; // acquire selection

      max_size = SendMessage(hwndEditControl, EM_GETLIMITTEXT, 0, 0);
      text_size = SendMessage(hwndEditControl, WM_GETTEXTLENGTH, 0, 0);

      *wr_buffer_ptr = '\0';

      end = ec_start + posit + n;

      // check if enough space (reserve room of "\r\n")
      if (end >= max_size) // else delete n and write n: nothing change !
	{
	  int line = SendMessage(hwndEditControl, EM_LINEFROMCHAR, end - max_size, 0);
	  int line_index = SendMessage(hwndEditControl, EM_LINEINDEX, line + 1, 0);
	  SendMessage(hwndEditControl, EM_SETSEL, 0, line_index);
	  SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) wr_buffer_ptr);  /* empty string to remove lines */
	  text_size = SendMessage(hwndEditControl, WM_GETTEXTLENGTH, 0, 0);
	  SendMessage(hwndEditControl, EM_SETSEL, text_size, text_size);
	  SendMessage(hwndEditControl, EM_GETSEL, (WPARAM) &ec_start, (WPARAM) &end); // re-init ec_start
	}

      SendMessage(hwndEditControl, EM_SETSEL, ec_start + posit, ec_start + posit + n);
      SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) wr_buffer);
      posit += n;
    }
  Set_Caret_Position(posit);

  wr_buffer_ptr = wr_buffer;
  last_is_read = 0;

  dont_use_selection = 0; // release selection
}


DLLEXPORT void
W32GC_Set_Line_Buffering(int is_buffered)
{
  line_buffering = is_buffered;

  if (!line_buffering)
    {
      Flush_Buffer();
      CheckMenuItem(GetMenu(hwndMain), IDM_BUFFERING, MF_BYCOMMAND | MF_UNCHECKED);
      EnableMenuItem(GetMenu(hwndMain), IDM_FLUSH, MF_BYCOMMAND | MF_GRAYED);
    }
  else
    {
      CheckMenuItem(GetMenu(hwndMain), IDM_BUFFERING, MF_BYCOMMAND | MF_CHECKED);
      EnableMenuItem(GetMenu(hwndMain), IDM_FLUSH, MF_BYCOMMAND | MF_ENABLED);
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

#ifdef DEBUG
static int
Console_Printf(char *format, ...)       /* debug: display in the GUI */
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
  if(n == 0)
    return;

  Flush_Buffer();               /* synchronize output and posit */
  posit -= n;
  Set_Caret_Position(posit);
}

DLLEXPORT void
W32GC_Forwd(int n)
{
  if(n == 0)
    return;

  Flush_Buffer();               /* synchronize output and posit */
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
  Flush_Buffer();               /* synchronize output and posit */
  Set_Selection(posit, n);
  SendMessage(hwndEditControl, EM_REPLACESEL, 0, (LPARAM) "");
}


DLLEXPORT void
W32GC_Emit_Beep()
{
  if (beep_on_error)
    PlaySound("SystemAsterisk", NULL, SND_ALIAS | SND_ASYNC);
  //    Beep(440, 100); // old
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
  oldFont = SelectObject(hDC, hFont);
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
  Flush_Buffer();               /* synchronize output and posit */

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
  sprintf(uniq_title, "%ld/%ld", GetTickCount(), GetCurrentProcessId());
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
  ShowWindow(hwnd_console, (show_console) ? SW_SHOW : SW_HIDE);
  SET_CHECKED_OPT(IDM_SHOW_CONSOLE, show_console);
}
