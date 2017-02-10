void (*pc)();

void *exp, *envr__m__cps, *k, *v, *y, *a, *cr__m__cps;

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_ex; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_xr1; void *_xr2; } _mult;
    struct { void *_x; } _subr1;
    struct { void *_x; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_ex; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *ex);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *xr1, void *xr2);
void *exprr_subr1(void *x);
void *exprr_zero(void *x);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *ex, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

void valuer__m__ofr__m__cps();
struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _emptyr__m__k_kt,
    _multr__m__innerr__m__k_kt,
    _multr__m__outerr__m__k_kt,
    _subr1r__m__k_kt,
    _zeror__m__k_kt,
    _ifr__m__k_kt,
    _throwr__m__k_kt,
    _letr__m__k_kt,
    _appr__m__innerr__m__k_kt,
    _appr__m__outerr__m__k_kt
  } tag;
  union {
    struct { void *_dismount; } _emptyr__m__k;
    struct { void *_vr__ex__; void *_kr__ex__; } _multr__m__innerr__m__k;
    struct { void *_xr2r__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _multr__m__outerr__m__k;
    struct { void *_kr__ex__; } _subr1r__m__k;
    struct { void *_kr__ex__; } _zeror__m__k;
    struct { void *_conseqr__ex__; void *_altr__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _ifr__m__k;
    struct { void *_vr__m__expr__ex__; void *_envr__m__cpsr__ex__; } _throwr__m__k;
    struct { void *_bodyr__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _letr__m__k;
    struct { void *_cr__m__cpsr__ex__; void *_kr__ex__; } _appr__m__innerr__m__k;
    struct { void *_randr__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _appr__m__outerr__m__k;
  } u;
};

void *ktr_emptyr__m__k(void *dismount);
void *ktr_multr__m__innerr__m__k(void *vr__ex__, void *kr__ex__);
void *ktr_multr__m__outerr__m__k(void *xr2r__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);
void *ktr_subr1r__m__k(void *kr__ex__);
void *ktr_zeror__m__k(void *kr__ex__);
void *ktr_ifr__m__k(void *conseqr__ex__, void *altr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);
void *ktr_throwr__m__k(void *vr__m__expr__ex__, void *envr__m__cpsr__ex__);
void *ktr_letr__m__k(void *bodyr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);
void *ktr_appr__m__innerr__m__k(void *cr__m__cpsr__ex__, void *kr__ex__);
void *ktr_appr__m__outerr__m__k(void *randr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);

void applyr__m__k();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _extend_envr,
    _emptyr__m__env_envr
  } tag;
  union {
    struct { void *_ar__ex__; void *_envr__m__cpsr__ex__; } _extend;
    struct { char dummy; } _emptyr__m__env;
  } u;
};

void *envrr_extend(void *ar__ex__, void *envr__m__cpsr__ex__);
void *envrr_emptyr__m__env();

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_bodyr__ex__; void *_envr__m__cpsr__ex__; } _closure;
  } u;
};

void *closr_closure(void *bodyr__ex__, void *envr__m__cpsr__ex__);

void applyr__m__closure();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

