//long mydata;

long bar(void);
/*
void foo(long mydata){
bar(mydata);
baz();
}
*/
#if 0
//static void __attribute__ ((constructor))
void
initializer_fct(void)
{
  //  bar(-1234567891234567);
  //
  //bar(0xABCD1234);
  if (bar() == -1412623820)
    foo();
  //  bar(-1234567);

}
#endif

void baz(long,double,long,long,long,long,long,long,long,long,long,long,long,long,long,double);

double d = 3.1415926535897932384626;
  int t[10];
void f() {
  bar5(t);
  baz(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,(double) 3.1415926535897932384626);
}

//void foo1(long);
void foo(double);

void g() {
  foo1(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,1.2,21);
}

static foo44(){
}

void fprep_cp() {
  f(&foo44);
}

void fstr() {
  f("toto", "titi", "tutu");
}

long mem;
long mem_array[100];
void fmem() {
  foo3(mem);
  foo3(mem_array[4102]);
}

void fmem_adr(){
  foo4(&mem);
  foo4(&mem_array[4096]);
}

double *fd;

void ffd(){
register double *fd asm("%l3");
  foo(fd[10]);
  foo(fd[4102]);
}
void ffd_adr(){
register double *fd asm("%l3");
  foo4(&fd[10]);
  foo4(&fd[4102]);
}

void test_call_c_lot_args();
static long ma_local_var2;

void flot(){
register long *X asm("%l0");
register long *Y asm("%l1");

 test_call_c_lot_args1(0,0,0,0,0,0,&test_call_c_lot_args,ma_local_var2,4095,123456789,-3.141593,"abcd\01489def\n\r",X[0],&X[0],X[255],&X[128],Y[0],&Y[0],Y[12],&Y[6], 1.23456);
}
