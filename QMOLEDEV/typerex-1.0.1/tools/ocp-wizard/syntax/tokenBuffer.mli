(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** Tokenized Gap buffers *)

module type TOKEN = sig

  (** The type of tokens, which should corresponding to a non-empty
      character range (so no EOF). *)
  type token

  (** A mutable context for the lexer (we use it to record the first
      unterminated string or comment, for example. *)
  type state

  val t_pad : token (** Used to fill the gap. May be anything *)
  val length : token -> int (** character length of the token *)
  val string : token -> string (** Used for debugging only *)
  val equals : token -> token -> bool

  (** [lexer state ~prefix_start ~modified_end ~find_ahead gb] must
      return a lexing function which will lexe the gap buffer [gb]
      starting from [prefix_start], and return a minimal number of
      lexed tokens at each application to [()]. The [length] of the
      returned tokens must match exactly the number of characters read
      since [prefix_start], so for example, white-space and comments
      should either increase the length of neighbouring tokens, or be
      returned themselves as tokens.

      The two parameters modified_end and find_ahead are provided to
      allow a smarter behavior of the lexer. *)
  val lexer :
    state ->
    prefix_start:int ->
    modified_end:int -> (** the gap_buffer is unchanged after this index *)
    find_ahead:((token -> bool) -> int option) ->
    (** find such a token starting at or after modified_end, and
        return its absolute char index in the gap_buffer. *)
    GapBuffer.gap_buffer ->
    unit -> token list

  (** Specifies that lexing should not start after this character
      position, even if the modified portion is far after. This is
      used for the recorded position of first unterminated string or
      comment. [max_int] is OK. *)
  val min_undo_start : GapBuffer.gap_buffer -> state -> int

end

module Make : functor (Token : TOKEN) -> sig

  (* 3.11 compatibility *)
  module type T = sig

  (** A tokenized buffer contains a char buffer, which should never be
      changed directly, plus a corresponding buffer of tokens, which
      is aligned with the character buffer. *)
  type tokenized_buffer = private {
    chars : GapBuffer.gap_buffer;
    mutable t_buf : Token.token array;
    (** Gap buffer of tokens, with their length. *)
    mutable t_pre : int;
    mutable t_post : int;
    mutable offset : int;
    (** position with respect to begining of the token t_buf.(t_post),
        in the interval [0, len-1] if len = |t_buf.(t_post)| *)
    state: Token.state
  }

  (** Create a tokenized buffer with given initial size for the
      character and token buffers, respectively (0 0 is safe). *)
  val create : int -> int -> Token.state -> tokenized_buffer

  (** Remove all the contents. *)
  val clear : tokenized_buffer -> unit

  (** Number of tokens in this buffer. *)
  val t_length : tokenized_buffer -> int

  val t_snapshot : tokenized_buffer -> out_channel -> unit

  (** Goto the specified character position. *)
  val goto : tokenized_buffer -> int -> unit

  val forward : tokenized_buffer -> unit
  val backward : tokenized_buffer -> unit

  (** Return the char position of the beginning of the token
      containing the given char position.

      Warning ! This moves the cursor. *)
  val pos_bot : tokenized_buffer -> int -> int

  (** Return the char position of the beginning of the first token
      starting at or after the given char position.

      Warning ! This moves the cursor. *)
  val pos_bont : tokenized_buffer -> int -> int

  (** [sub buf b e] returns the segment "[b,e[" of [buf]. *)
  val sub : tokenized_buffer -> int -> int -> Token.token array

  val pos2pointer : tokenized_buffer -> int -> int

  (** [replace tb count s min_undo_start] replaces in [tb] the next
      [count] characters by [s], and returns
      - the starting position in characters of the modified tokens
      - the number of chars corresponding to the removed tokens
      - the new tokens. *)
  val replace :
    tokenized_buffer -> int -> string -> int * int * Token.token array

  (* 3.11 compatibility *)
  end
  include T

end
