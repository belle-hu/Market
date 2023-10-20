module type StoreType = sig
    type 'a t = 'a list
    val count_products: 'a t -> int
    val all_products: 'a t -> 'a t
    val import_goods: 'a t -> 'a t
    val sell_goods: 'a t -> 'a t
    val popular_goods: 'a t -> 'a t
    val on_sale_goods: 'a t -> 'a t
    val in_stock_goods: 'a t -> 'a t
    val out_of_stock_goods: 'a t
  end

  (** Sanitize a string by removing non-alphanumeric symbols. Return the
    list of words obtained by splitting on spaces. *)
val sanitize : string -> string list

(** Build a random Ngram model from a training corpus consisting of a
    list of words, and an integer parameter [n]. Requires: [n > 0]. *)
val build_rand_ngram : string -> int -> Rand_ngram.t

(** Build a most-frequent Ngram model from a training corpus consisting of a
    list of words, and an integer parameter [n]. Requires: [n > 0]. *)
val build_freq_ngram : string -> int -> Freq_ngram.t

(** Build an interpolated Ngram model from a training corpus consisting of a
    list of words, and an integer parameter [n]. Requires: [n > 1]. *)
val build_interp_ngram : string -> int -> Interp_ngram.t

(** Given a maximum number [N] of words to generate and a prompt consisting of a
    list of words, generate up to [N] words using the random Ngram model.
    Requires: [N >= 0]. *)
val create_rand_sequence : Rand_ngram.t -> int -> string -> string

(** Given a maximum number [N] of words to generate and a prompt consisting of a
    list of words, generate up to [N] words using the most-frequent Ngram model.
    Requires: [N >= 0]. *)
val create_freq_sequence : Freq_ngram.t -> int -> string -> string

(** Given a maximum number [N] of words to generate and a prompt consisting of a
    list of words, generate up to [N] words using the most-frequent Ngram model.
    Requires: [N >= 0]. *)
val create_interp_sequence : Interp_ngram.t -> int -> string -> string
