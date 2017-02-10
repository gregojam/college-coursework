#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *closr_closure(void *bodyr__ex__, void *envr__m__cpsr__ex__) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._bodyr__ex__ = bodyr__ex__;
  _data->u._closure._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  return (void *)_data;
}

void *envrr_extend(void *ar__ex__, void *envr__m__cpsr__ex__) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extend_envr;
  _data->u._extend._ar__ex__ = ar__ex__;
  _data->u._extend._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  return (void *)_data;
}

void *envrr_emptyr__m__env() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__env_envr;
  return (void *)_data;
}

void *ktr_emptyr__m__k(void *dismount) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._dismount = dismount;
  return (void *)_data;
}

void *ktr_multr__m__innerr__m__k(void *vr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__innerr__m__k_kt;
  _data->u._multr__m__innerr__m__k._vr__ex__ = vr__ex__;
  _data->u._multr__m__innerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_multr__m__outerr__m__k(void *xr2r__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__outerr__m__k_kt;
  _data->u._multr__m__outerr__m__k._xr2r__ex__ = xr2r__ex__;
  _data->u._multr__m__outerr__m__k._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._multr__m__outerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_subr1r__m__k(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1r__m__k_kt;
  _data->u._subr1r__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_zeror__m__k(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zeror__m__k_kt;
  _data->u._zeror__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_ifr__m__k(void *conseqr__ex__, void *altr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _ifr__m__k_kt;
  _data->u._ifr__m__k._conseqr__ex__ = conseqr__ex__;
  _data->u._ifr__m__k._altr__ex__ = altr__ex__;
  _data->u._ifr__m__k._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._ifr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_throwr__m__k(void *vr__m__expr__ex__, void *envr__m__cpsr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__k_kt;
  _data->u._throwr__m__k._vr__m__expr__ex__ = vr__m__expr__ex__;
  _data->u._throwr__m__k._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  return (void *)_data;
}

void *ktr_letr__m__k(void *bodyr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letr__m__k_kt;
  _data->u._letr__m__k._bodyr__ex__ = bodyr__ex__;
  _data->u._letr__m__k._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._letr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_appr__m__innerr__m__k(void *cr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__innerr__m__k_kt;
  _data->u._appr__m__innerr__m__k._cr__m__cpsr__ex__ = cr__m__cpsr__ex__;
  _data->u._appr__m__innerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_appr__m__outerr__m__k(void *randr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__outerr__m__k_kt;
  _data->u._appr__m__outerr__m__k._randr__ex__ = randr__ex__;
  _data->u._appr__m__outerr__m__k._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._appr__m__outerr__m__k._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *ex) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._ex = ex;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *xr1, void *xr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._xr1 = xr1;
  _data->u._mult._xr2 = xr2;
  return (void *)_data;
}

void *exprr_subr1(void *x) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._x = x;
  return (void *)_data;
}

void *exprr_zero(void *x) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._x = x;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *ex, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._ex = ex;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
exp = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
envr__m__cps = (void *)envrr_emptyr__m__env();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Fact 5: %d\n", (int)v);}

void applyr__m__closure()
{
clos* _c = (clos*)cr__m__cps;
switch (_c->tag) {
case _closure_clos: {
void *bodyr__ex__ = _c->u._closure._bodyr__ex__;
void *envr__m__cpsr__ex__ = _c->u._closure._envr__m__cpsr__ex__;
exp = (void *)bodyr__ex__;
envr__m__cps = (void *)envrr_extend(a,envr__m__cpsr__ex__);
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)envr__m__cps;
switch (_c->tag) {
case _emptyr__m__env_envr: {
fprintf(stderr, "unbound identifier");
 exit(1);
break; }
case _extend_envr: {
void *ar__ex__ = _c->u._extend._ar__ex__;
void *envr__m__cpsr__ex__ = _c->u._extend._envr__m__cpsr__ex__;
if((y == 0)) {
  v = (void *)ar__ex__;
pc = &applyr__m__k;

} else {
  envr__m__cps = (void *)envr__m__cpsr__ex__;
y = (void *)(void *)((int)y - 1);
pc = &applyr__m__env;

}
break; }
}
}

void applyr__m__k()
{
kt* _c = (kt*)k;
switch (_c->tag) {
case _emptyr__m__k_kt: {
void *dismount = _c->u._emptyr__m__k._dismount;
_trstr *trstr = (_trstr *)dismount;
longjmp(*trstr->jmpbuf, 1);
break; }
case _multr__m__innerr__m__k_kt: {
void *vr__ex__ = _c->u._multr__m__innerr__m__k._vr__ex__;
void *kr__ex__ = _c->u._multr__m__innerr__m__k._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(void *)((int)vr__ex__ * (int)v);
pc = &applyr__m__k;
break; }
case _multr__m__outerr__m__k_kt: {
void *xr2r__ex__ = _c->u._multr__m__outerr__m__k._xr2r__ex__;
void *envr__m__cpsr__ex__ = _c->u._multr__m__outerr__m__k._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._multr__m__outerr__m__k._kr__ex__;
exp = (void *)xr2r__ex__;
envr__m__cps = (void *)envr__m__cpsr__ex__;
k = (void *)ktr_multr__m__innerr__m__k(v,kr__ex__);
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1r__m__k_kt: {
void *kr__ex__ = _c->u._subr1r__m__k._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(void *)((int)v - 1);
pc = &applyr__m__k;
break; }
case _zeror__m__k_kt: {
void *kr__ex__ = _c->u._zeror__m__k._kr__ex__;
k = (void *)kr__ex__;
v = (void *)(v == 0);
pc = &applyr__m__k;
break; }
case _ifr__m__k_kt: {
void *conseqr__ex__ = _c->u._ifr__m__k._conseqr__ex__;
void *altr__ex__ = _c->u._ifr__m__k._altr__ex__;
void *envr__m__cpsr__ex__ = _c->u._ifr__m__k._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._ifr__m__k._kr__ex__;
if(v) {
  exp = (void *)conseqr__ex__;
envr__m__cps = (void *)envr__m__cpsr__ex__;
k = (void *)kr__ex__;
pc = &valuer__m__ofr__m__cps;

} else {
  exp = (void *)altr__ex__;
envr__m__cps = (void *)envr__m__cpsr__ex__;
k = (void *)kr__ex__;
pc = &valuer__m__ofr__m__cps;

}
break; }
case _throwr__m__k_kt: {
void *vr__m__expr__ex__ = _c->u._throwr__m__k._vr__m__expr__ex__;
void *envr__m__cpsr__ex__ = _c->u._throwr__m__k._envr__m__cpsr__ex__;
exp = (void *)vr__m__expr__ex__;
envr__m__cps = (void *)envr__m__cpsr__ex__;
k = (void *)v;
pc = &valuer__m__ofr__m__cps;
break; }
case _letr__m__k_kt: {
void *bodyr__ex__ = _c->u._letr__m__k._bodyr__ex__;
void *envr__m__cpsr__ex__ = _c->u._letr__m__k._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._letr__m__k._kr__ex__;
exp = (void *)bodyr__ex__;
envr__m__cps = (void *)envrr_extend(v,envr__m__cpsr__ex__);
k = (void *)kr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _appr__m__innerr__m__k_kt: {
void *cr__m__cpsr__ex__ = _c->u._appr__m__innerr__m__k._cr__m__cpsr__ex__;
void *kr__ex__ = _c->u._appr__m__innerr__m__k._kr__ex__;
cr__m__cps = (void *)cr__m__cpsr__ex__;
a = (void *)v;
k = (void *)kr__ex__;
pc = &applyr__m__closure;
break; }
case _appr__m__outerr__m__k_kt: {
void *randr__ex__ = _c->u._appr__m__outerr__m__k._randr__ex__;
void *envr__m__cpsr__ex__ = _c->u._appr__m__outerr__m__k._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._appr__m__outerr__m__k._kr__ex__;
exp = (void *)randr__ex__;
envr__m__cps = (void *)envr__m__cpsr__ex__;
k = (void *)ktr_appr__m__innerr__m__k(v,kr__ex__);
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)exp;
switch (_c->tag) {
case _const_expr: {
void *ex = _c->u._const._cexp;
v = (void *)ex;
pc = &applyr__m__k;
break; }
case _var_expr: {
void *ex = _c->u._var._ex;
y = (void *)ex;
pc = &applyr__m__env;
break; }
case _mult_expr: {
void *xr1 = _c->u._mult._xr1;
void *xr2 = _c->u._mult._xr2;
exp = (void *)xr1;
k = (void *)ktr_multr__m__outerr__m__k(xr2,envr__m__cps,k);
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *x = _c->u._subr1._x;
exp = (void *)x;
k = (void *)ktr_subr1r__m__k(k);
pc = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *x = _c->u._zero._x;
exp = (void *)x;
k = (void *)ktr_zeror__m__k(k);
pc = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
exp = (void *)test;
k = (void *)ktr_ifr__m__k(conseq,alt,envr__m__cps,k);
pc = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
exp = (void *)body;
envr__m__cps = (void *)envrr_extend(k,envr__m__cps);
pc = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *kexp = _c->u._throw._kexp;
void *vexp = _c->u._throw._vexp;
exp = (void *)kexp;
k = (void *)ktr_throwr__m__k(vexp,envr__m__cps);
pc = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *ex = _c->u._let._ex;
void *body = _c->u._let._body;
exp = (void *)ex;
k = (void *)ktr_letr__m__k(body,envr__m__cps,k);
pc = &valuer__m__ofr__m__cps;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
v = (void *)closr_closure(body,envr__m__cps);
pc = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
exp = (void *)rator;
k = (void *)ktr_appr__m__outerr__m__k(rand,envr__m__cps,k);
pc = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
k= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
pc();
}
}
return 0;
}
