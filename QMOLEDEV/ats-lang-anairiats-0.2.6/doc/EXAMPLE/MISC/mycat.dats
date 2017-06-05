(*
**
** An implementation of 'cat', kind of
**
** Author: Sylvain Nahas (* sylvain DOT nahas AT googlemail DOT com *)
** Author: Hongwei Xi (* hwxi AT cs DOT bu DOT edu *)
*)

(* ****** ****** *)

staload
UN = "prelude/SATS/unsafe.sats"

staload
FCNTL = "libc/SATS/fcntl.sats"
stadef fildes_v = $FCNTL.fildes_v
macdef O_RDONLY = $FCNTL.O_RDONLY

staload
UNISTD = "libc/SATS/unistd.sats"
macdef STDIN_FILENO = $UNISTD.STDIN_FILENO
macdef STDOUT_FILENO = $UNISTD.STDOUT_FILENO

(* ****** ****** *)

extern
fun{env:viewt@ype}
getchars {n:nat} (
  env: &env, buf: &bytes(n), n: size_t (n)
) : sizeLte (n) // end of [getchars]

extern
fun{env:viewt@ype}
putchars {n:nat} {n1:nat | n1 <= n}
  (env: &env, buf: &bytes(n), n1: size_t n1): void
// end of [putchars]

fun{env1,env2:viewt@ype}
catloop {n:nat} (
  env1: &env1, env2: &env2
, buf: &bytes(n), n: size_t n
) : void = let
  val n1 = getchars<env1> (env1, buf, n)
in
  if n1 > 0 then
    (putchars<env2> (env2, buf, n1); catloop (env1, env2, buf, n))
  else () // end of [if]
end // end of [catloop]

;(* ****** ****** *)

%{^
typedef struct {
  int number_nonblank ;
  int show_ends ;
  int number ;
  int squeeze_blank ;
  int show_tabs ;
  int show_nonprinting ;
} params_t ;
%}
typedef
params =
$extype_struct
  "params_t" of {
  number_nonblank = bool,	// number nonempty output lines
  show_ends = bool,     	// display $ at end of each line
  number = bool,        	// number all output lines
  squeeze_blank= bool,		// suppress repeated empty output lines
  show_tabs=bool,       	// display TAB characters as ^I
  show_nonprinting=bool		// use ^ and M- notation, except for LFD and TAB
}

extern
fun params_copy (
  to: &params, from: &params
) : void = "mycat_params_copy"
implement
params_copy
  (to, from) = {
  val () = to.number_nonblank := from.number_nonblank
  val () = to.show_ends := from.show_ends
  val () = to.number := from.number
  val () = to.squeeze_blank := from.squeeze_blank
  val () = to.show_tabs := from.show_tabs
  val () = to.show_nonprinting := from.show_nonprinting
} // end of [params_copy]

(* ****** ****** *)

fn is_quoted_output
  (params: &params): bool =
   params.number_nonblank
|| params.show_ends
|| params.number
|| params.squeeze_blank
|| params.show_tabs
|| params.show_nonprinting

(* ****** ****** *)

%{^
typedef
struct {
  int fildes ;
} envinp_t ;
%}
viewtypedef
envinp (fd:int) =
$extype_struct "envinp_t" of {
  fildes= int (fd) // file descriptor
, fildes_v= fildes_v (fd) // file descriptor view
} // end of [envinp]

%{^
typedef
struct {
  int dummy ;
} envstdout_t ;
%}
viewtypedef
envstdout () =
$extype_struct "envstdout_t" of {
  dummy= int
}

(* ****** ****** *)

local
sta fd: int
in
implement
getchars<envinp(fd)> (env, buf, n) = let
  val n1 = $FCNTL.read_exn (env.fildes_v | env.fildes, buf, n)
in
  n1 (* the number of chars read *)
end // end of [end]
end // end of [local]

implement
putchars<envstdout()> (env, buf, n1) = let
  val (pf_stdout | ()) = $UNISTD.stdout_fildes_view_get ()
  val n1 = $FCNTL.write_all_err (pf_stdout | STDOUT_FILENO, buf, n1)
  val () = $UNISTD.stdout_fildes_view_set (pf_stdout | (*none*))
in
  // nothing
end // end of [putchars]

(* ****** ****** *)

fun readout_raw
  {fd:int} (
  pf: !fildes_v fd | fd: int fd
) : void = let
//
  var env1: envinp(fd)
  val () = env1.fildes := fd
  prval () = env1.fildes_v := pf
  var env2: envstdout ()
  val () = env2.dummy := 0
//
  #define BUFSZ 4096
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  val () = catloop<envinp(fd),envstdout()> (env1, env2, !p_buf, BUFSZ)
//
  prval () = pf := env1.fildes_v
in
  // nothing
end // end of [readout_raw]

(* ****** ****** *)

absview cbuf_v (l0:addr, n: int, l:addr)

%{^
ATSinline()
ats_void_type
cbuf_putchar (
  ats_ptr_type p
, ats_char_type c
) {
  *(char*)p = c ; return ;
} // end of [cbuf_putchar]

ATSinline()
ats_void_type
cbuf_clearall (
  ats_ptr_type p0, ats_ptr_type p
) {
  if (p0 < p) {
    atslib_fildes_write_all_exn (STDOUT_FILENO, p0, (char*)p - (char*)p0) ;
  } // end of [if]
  return ;
} // end of [cbuf_clearall]
%} // end of [%{^]

extern
fun cbuf_putchar
  {n:nat}
  {l0:addr}
  {l:addr | l < l0 + n} (
  pf: !cbuf_v (l0, n, l) >> cbuf_v (l0, n, l+1)
| p: ptr l, c: char
) : void = "mac#cbuf_putchar"

extern
fun cbuf_clearall
  {n:nat}
  {l0:addr}
  {l:addr | l <= l0 + n} (
  pf: !cbuf_v (l0, n, l) >> cbuf_v (l0, n, l0)
| p0: ptr l0, p: ptr l
) : void = "mac#cbuf_clearall"

(* ****** ****** *)

fun putchar_quoted_buf
  {n:nat}
  {l0:addr}
  {l:addr | l + 4 <= l0 + n} (
  pfbuf: !cbuf_v (l0, n, l) >> cbuf_v (l0, n, l)
| params: &params, b: char, p0: ptr l0, p: ptr l
) : #[l:addr | l <= l0+n] ptr l = let
  #define i2c char_of_int
  val ch = int_of_uchar ((uchar_of_char)b)
  macdef putc (p, c) = cbuf_putchar (pfbuf | ,(p), ,(c))
in
  case+ 0 of
  | _ when ch < 32 => begin case+ b of
    | '\t' when ~params.show_tabs => (putc(p, '\t'); p+1)
    | '\n' => (putc(p, '$') ; putc(p+1, '\n'); p+2)
    | _ => (putc(p, '^'); putc(p+1, i2c(ch+64)); p+2)
    end // end of [...]
  | _ when ch < 127 => ( putc (p, b); p+1 )
  | _ when ch = 127 => ( putc (p, '^'); putc (p+1, '?'); p+2 )
  | _ when ch < 128 + 32  => (
      putc(p, 'M'); putc(p+1, '-'); putc(p+2, '^'); putc(p+3, i2c(ch-128+64)); p+4
    )
  | _ when ch < 128 + 127 => (
      putc(p, 'M'); putc(p+1, '-'); putc(p+2, i2c(ch-128)); p+3
    )
  | _ => (
      putc(p, 'M'); putc(p+1, '-'); putc(p+2, '^'); putc(p+3, '?'); p+4
    ) (* end of [_] *)
end // end of [putchar_quoted_buf]

(* ****** ****** *)

%{^
#define CBUFSZ 4096
%}
#define CBUFSZ 4096

fun putchars_quoted
  {n:int}
  {n1,i:nat | i <= n1; n1 <= n}
  {l0:addr} {l:addr | l <= l0+CBUFSZ} (
  pfbuf: !cbuf_v (l0, CBUFSZ, l) >> cbuf_v (l0, CBUFSZ, l0) 
| params: &params
, cs: &bytes(n), n1: size_t n1, i: size_t i, p0: ptr l0, p: ptr l
) : void =
  if i < n1 then let
    val b = (char_of_byte)cs.[i]
  in
    if p + 4 <= p0 + CBUFSZ then let
      val p = putchar_quoted_buf (pfbuf | params, b, p0, p)
    in
      putchars_quoted (pfbuf | params, cs, n1, i+1, p0, p)
    end else let
      val () = cbuf_clearall (pfbuf | p0, p)
      val p = putchar_quoted_buf (pfbuf | params, b, p0, p0)
    in
      putchars_quoted (pfbuf | params, cs, n1, i+1, p0, p)
    end
  end else let
    val () = cbuf_clearall (pfbuf | p0, p)
  in
    // nothing
  end // end of [if]

(* ****** ****** *)

%{^
typedef struct {
  params_t params ;
  ats_ptr_type p_cbuf ;
} envstdoutq_t ;

ATSinline()
ats_void_type
envstdoutq_initialize (
  envstdoutq_t *env, params_t *from
) {
//
extern
ats_void_type
mycat_params_copy (ats_ptr_type to, ats_ptr_type from) ;
//
  mycat_params_copy (&env->params, from) ;
  env->p_cbuf = ATS_MALLOC (CBUFSZ) ;
  return ;
} // end of [envstdoutq_initialize]

ATSinline()
ats_void_type
envstdoutq_uninitialize (
  envstdoutq_t *env
) {
  ATS_FREE (env->p_cbuf) ; return ;
} // end of [envstdoutq_uninitialize]

ATSinline()
ats_ptr_type
envstdoutq_get_cbuf
  (envstdoutq_t *env) { return env->p_cbuf ; }
// end of [envstdoutq_get_cbuf]

%} // end of [%{^]

(* ****** ****** *)

viewtypedef
envstdoutq =
$extype_struct
  "envstdoutq_t" of {
  params= params
, _rest= undefined_vt
}

extern
fun envstdoutq_initialize (
  env: &envstdoutq? >> envstdoutq, from: &params
) : void = "mac#envstdoutq_initialize"
extern
fun envstdoutq_uninitialize (
  env: &envstdoutq >> envstdoutq?
) : void = "mac#envstdoutq_uninitialize"

local

stadef viewout = $UN.viewout

in // in of [local]

extern
fun envstdoutq_get_cbuf
  (env: &envstdoutq)
  : [l0:addr] (viewout (cbuf_v (l0, CBUFSZ, l0)) | ptr l0)
  = "mac#envstdoutq_get_cbuf"

implement
putchars<envstdoutq> (env, cs, n1) = let
  val (pfout | p0) = envstdoutq_get_cbuf (env)
  prval (pf, fpf) = $UN.viewout_decode (pfout)
  val () = putchars_quoted (pf | env.params, cs, n1, 0, p0, p0)
  prval () = fpf (pf)
in
  // nothing
end // end of [putchars]

end // end of [

(* ****** ****** *)

fun readout_quoted
  {fd:int} (
  pf: !fildes_v fd | params: &params, fd: int fd
) : void = let
//
  var env1: envinp(fd)
  val () = env1.fildes := fd
  prval () = env1.fildes_v := pf
  var env2: envstdoutq
  val () = envstdoutq_initialize (env2, params)
//
  #define BUFSZ 4096
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  val () = catloop<envinp(fd),envstdoutq> (env1, env2, !p_buf, BUFSZ)
//
  prval () = pf := env1.fildes_v
  val () = envstdoutq_uninitialize (env2)
in
  // nothing
end // end of [readout_quoted]

(* ****** ****** *)

fun cat_stdin
  (params: &params): void = {
  val (pf_stdin | ()) = $UNISTD.stdin_fildes_view_get ()
  val isq = is_quoted_output (params)
  val () =
    if isq then
      readout_quoted (pf_stdin | params, STDIN_FILENO)
    else
      readout_raw (pf_stdin | STDIN_FILENO) // output directly
    // end of [if]
  val () = $UNISTD.stdin_fildes_view_set (pf_stdin | (*none*))
} // end of [cat_stdin]

(* ****** ****** *)
/*
** open, dump and close file
*/
fun cat_file (
  params: &params, path: string
) : void = let
  val (pf_fd | fd) = $FCNTL.open_flag_exn(path, O_RDONLY)
  val isq = is_quoted_output (params)
  val () =
    if isq then
      readout_quoted (pf_fd | params, fd)
    else
      readout_raw (pf_fd | fd) // output directly
    // end of [if]
in
  $FCNTL.close_exn (pf_fd | fd)
end // end of [cat_file]

(* ****** ****** *)

(* an "object-like" notation would be cool! *)
exception FunctionalityNotSupported
fn number_nonblank(params: &params) :<!exn> void = $raise FunctionalityNotSupported
fn show_ends(params: &params) : void = params.show_ends := true
fn number(params: &params) :<!exn> void = $raise FunctionalityNotSupported
fn squeeze_blank(params: &params) :<!exn> void = $raise FunctionalityNotSupported
fn show_tabs(params: &params) : void = params.show_tabs := true
fn show_nonprinting (params: &params) : void = params.show_nonprinting := true

(* ****** ****** *)

#define PROGRAM_VERSION "6.10\n"
fn version() = prerr(PROGRAM_VERSION)

(* ****** ****** *)

fn help () = (
prerr "Usage: cat [OPTION] [FILE]...
Concatenate FILE(s), or standard input, to standard output.
	-A, --show-all           equivalent to -vET
	-b, --number-nonblank    number nonempty output lines
	-e                       equivalent to -vE
	-E, --show-ends          display $ at end of each line
	-n, --number             number all output lines
	-s, --squeeze-blank      suppress repeated empty output lines
	-t                       equivalent to -vT
	-T, --show-tabs          display TAB characters as ^I
	-u                       (ignored)
	-v, --show-nonprinting   use ^ and M- notation, except for LFD and TAB
	--help     display this help and exit
	--version  output version information and exit
With no FILE, or when FILE is -, read standard input.
Examples:
  cat f - g  Output f's contents, then standard input, then g's contents.
  cat        Copy standard input to standard output.\n"
)

(* ****** ****** *)
/*
** SN: suppose that current parameter is a file path to cat()
*/
fun parse_file_path
  {n:int | n >= 1}
  {m:nat | m < n } (
  params: &params, argc: int n, argv: &(@[string][n]), current: int m
) : void = let
  val () = assert(current < argc)
  val path = string1_of_string (argv.[current])
  val () = assert_errmsg(
    gte_size1_int1(string1_length(path),1),"path must contain at least one character!"
  ) // end of [val]
  val () = cat_file(params, path)
  val next = current+1
in
  if next = argc then
    ()
  else
    parse_file_path(params, argc, argv, next)
  // end of [if]
end (* end of [parse_file_path] *)

(* ****** ****** *)
/*
** SN: suppose that current parameter is not a file path
*/
fun parse_non_file_parameters
  {n:int | n >= 1}
  {m:nat | m < n } (
  params: &params, argc: int n, argv: &(@[string][n]), current: int m
) : bool = let
  val () = assert(current < argc)
  val param = string1_of_string (argv.[current])
in
  case+ param of
  | "--help" => ( help(); exit(0); )
  | "--version" => ( version(); exit(0); )
  | "-A" => ( show_nonprinting(params); show_ends(params); show_tabs(params); false; )
  | "--show-all" => ( show_nonprinting(params); show_ends(params); show_tabs(params); false; )
  | "-b" => ( number_nonblank(params); false; )
  | "--number-nonblank" => ( number_nonblank(params); false; )
  | "-e" => ( show_nonprinting(params); show_ends(params); false; )
  | "-E" => ( show_ends(params); false; )
  | "--show-ends" => ( show_ends(params); false; )
  | "-n" => ( number(params); false; )
  | "--number" => ( number(params); false; )
  | "-s" => ( squeeze_blank(params); false; )
  | "--squeeze-blank" => ( squeeze_blank(params); false; )
  | "-t" => ( show_tabs(params) ; show_nonprinting(params); false; )
  | "-T" => ( show_tabs(params); false; )
  | "--show-tabs" => ( show_tabs(params); false; )
  | "-u" => (* IGNORED*) false
  | "-v" => ( show_nonprinting(params); false; )
  | "--show-nonprinting" => ( show_nonprinting(params); false; )
  | _ => (* unknown parameter => file path? *) true
end // end of [parse_non_file_parameters]

(* ****** ****** *)

fun parse_parameters
  {n:int | n >= 1}
  {m:nat | m < n } (
  params: &params, argc: int n, argv: &(@[string][n]), current: int m
) : void = let
  val isfilepath = parse_non_file_parameters (params, argc, argv, current)
  val next = current + 1
in
  case+ isfilepath of
  | false => (
      if next = argc then
        cat_stdin(params) else parse_parameters(params, argc,argv,next)
      // end of [if]
    ) // end of [false]
  | true => parse_file_path(params, argc,argv,current)
end // end of [parse_parameters]

(* ****** ****** *)

implement
main(argc, argv) = let
  var params : params
  val () = params.number_nonblank := false
  val () = params.show_ends := false
  val () = params.number := false
  val () = params.squeeze_blank:= false
  val () = params.show_tabs := false
  val () = params.show_nonprinting := false
in
  if argc = 1 then
    cat_stdin(params)
  else
    parse_parameters(params, argc,argv,1)
  // end of [if]
end (* end of [main] *)

(* ****** ****** *)

(* end of [mycat.dats] *)
