(** Generic log-cycling pipeline for long-running processes.

    Maintains a hierarchy of log files (L0, L1, L2, ...) where each level
    holds at most 100 entries (50 oldest + 50 newest). When a level overflows,
    its oldest 50 entries are compressed (keep every 10th = ~5) and cascaded
    to the next level. Levels are created lazily as data first cascades into them.

    Example: after 1M lines, L0 holds ~50 recent entries, L1 holds ~95
    (compressed from L0's oldest), L2 holds ~5, etc. Each file stays bounded
    while preserving long-term history at decreasing resolution.

    Autoflushes to disk after every [add_line] for crash safety. *)
type t

(** Create a new cycling log pipeline.

    All levels share the same CSV [header] line. No files are created yet —
    L0 is instantiated lazily on the first [add_line]. Files are written to
    [dir] (if given) with names like [logs-<stamp>-L0.csv],
    [logs-<stamp>-L1.csv], etc. When omitted, files are written to the
    current directory.
    The stamp should match the session start timestamp (same as used for
    solution files). *)
val create : stamp:string -> header:string -> ?dir:string -> unit -> t

(** Add a line to the pipeline (appended to L0).

    Triggers overflow/compression if L0 exceeds 100 entries: the oldest 50
    are compressed and cascaded to L1, which may itself overflow further up
    the chain. All levels are autoflushed to disk after each call. *)
val add_line : t -> string -> unit

(** Close the pipeline, flushing and closing all open file channels.

    Optional in practice — [add_line] already autoflushes for crash safety,
    but this ensures a clean shutdown (e.g. on Ctrl-C or normal exit). *)
val close : t -> unit
