//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

// book: AUP (2nd edition), pages 80 - 85

(* ****** ****** *)

staload "libc/sys/SATS/types.sats"

staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/random.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

#define LOCKDIR "/tmp/"

%{^
#define LOCKDIR "/tmp/"
%} // end of [%{^]

(* ****** ****** *)

#define MAXTRIES 10
#define NAPLENGTH 2

(* ****** ****** *)

%{^

static
ats_ptr_type lockpath (ats_ptr_type name) {
  static char path[100] ;
  if (snprintf (path, sizeof(path), "%s%s", LOCKDIR, (char*)name) > sizeof(path))
    return (char*)0;
  return path ;
} // end of [lockpath]

%} // end of [%{^]

extern
fun lockpath (name: !READ(string)): Stropt = "lockpath"

(* ****** ****** *)

extern fun lock (name: !READ(string)): bool
extern fun unlock (name: !READ(string)): bool

(* ****** ****** *)

macdef errno_is_EAGAIN () = (errno_get () = EAGAIN)
macdef errno_is_EEXIST () = (errno_get () = EEXIST)

(* ****** ****** *)

implement lock (name) = let
  val path = lockpath (name)
  val ans = stropt_is_some (path)
in
  if ans then let
    val path = stropt_unsome (path)
    val flag = O_WRONLY lor O_CREAT lor O_EXCL
    val err = loop (path, flag, 0) where {
      fun loop (
          path: !READ(string), flag: flag_t, n: int
        ) : int(*err*) = let
        val (pf_fdopt | fd) = open_flag_err (path, flag)
      in
        if (fd >= 0) then let
          prval open_v_succ (pf_fd) = pf_fdopt
          val () = close_loop_exn (pf_fd | fd)
        in
          0(*success*)
        end else let
          prval open_v_fail () = pf_fdopt
        in
          if errno_is_EEXIST () then
            (if n >= MAXTRIES - 1 then (errno_set EAGAIN; ~1) else loop (path, flag, n+1))
          else ~1(*failure*) 
        end (* end of [if] *)
      end // end of [loop]
    } // end of [val] 
  in
    if err = 0 then true else false
  end else false
end // end of [lock]

(* ****** ****** *)

implement unlock (name) = let
  val path = lockpath (name)
  val ans = stropt_is_some (path)
in
  if ans then let
    val path = stropt_unsome (path)
    val err = unlink (path)
  in
    if err <> ~1 then true else false
  end else false
end // end of [unlock]

(* ****** ****** *)

fn testlock (): void = loop (1) where {
  #define N 4
  #define NAME "accounts"
  fun loop (i: natLte N): void = let
    val status = (if lock (NAME) then let
      val pid = getpid ()
      val () = printf ("Process %ld acquired the lock\n", @(lint_of_pid pid))
      val _leftover = sleep (randint 5 + 1); // work on the accounts
      val ans = unlock (NAME)
      val () = if ~ans then (prerr "Exit: [testlock] failed"; exit 1)
    in
      1(* succ *)
    end else let
      val () = if errno_is_EAGAIN () then let
        val pid = getpid ()
        val () = printf ("Process %ld tired of busy waiting\n", @(lint_of_pid pid))
        val () = errno_reset ()
      in
        // nothing
      end else (prerr "Exit: [testlock] failed"; exit 1)
    in
      0(* fail *)
    end) : natLte 2 // end of [val]
    val _leftover = sleep (randint 5 + 5) // work on somthing else
    val i = i + status
  in
    if i <= N then loop (i) else ()
  end // end of [loop]
} // end of [testlock]

(* ****** ****** *)

implement main () = testlock ()

(* ****** ****** *)

(* end of [AUP_2_4_3.dats] *)
