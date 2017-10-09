#lang scribble/manual
@(require (for-label racket/base
                     (only-in racket/match match define-match-expander prop:match-expander))
          port-match)

@title{Pattern Matching for Ports}

@defmodule[port-match/base]

@defform[(port-match in-port clause ...)
         #:grammar ([clause [pat body ...+]])]{
Like @racket[match], but matches against values read from @racket[in-port].

@racket[pat] must be a @italic{head pattern}, which may be specified as follows:

@itemize[

 @item{@racket[(#,(racketidfont "byte") tail ...)] --- matches a single byte.}
 @item{@racket[(#,(racketidfont "bytes") amt tail ...)] --- matches a sequence
       of exactly @racket[amt] bytes.}
 @item{@racket[(#,(racketidfont "char") tail ...)] --- matches a single UTF-8 character.}
 @item{@racket[(#,(racketidfont "string") len tail ...)] --- matches a sequence
       of exactly @racket[len] characters.}
 @item{@racket[(#,(racketidfont "regexp") pattern tail ...)] --- matches a sequence
       of characters or bytes which match @racket[pattern]. The pattern must match
       immediately to count as a match (as if it began with @code{^}).}
 @item{@racketidfont{eof} --- matches end-of-file.}
 @item{@racket[(#,(racketidfont "seq") head ...)] --- matches each @racket[head] in sequence.}

]

Head patterns recognize one or more @italic{datums}, which may be matched against
@italic{tail patterns}:

@itemize[

 @item{@racket[(#,(racketidfont "app") proc tail)] --- calls @racket[proc] with the
       matched datum, and matches the result against @racket[tail].}
 @item{@racket[(#,(racketidfont "?") proc tail ...)] --- matches only if
       @racket[proc] returns true when applied to the matched datum.}
 @item{@racket[id] --- binds @racket[id] to the matched datum.}

]

Patterns may be combined as follows:

@itemize[

 @item{@racket[(#,(racketidfont "and") pat ...)] --- matches only if every @racket[pat] matches.}
 @item{@racket[(#,(racketidfont "or") pat ...)] --- matches if at least one @racket[pat] matches.}
 @item{@racketidfont{_} --- matches anything.}

]
}

@section{Extending @racket[port-match]}

@defform[(define-port-match-expander id proc-expr)]{
Similar to @racket[define-match-expander].
}

@defthing[prop:port-match-expander struct-type-property?]{
Similar to @racket[prop:match-expander].
}

@defproc[(port-match-expander? [v any/c]) boolean?]{
Predicate for values which implement @racket[prop:port-match-expander].
}
