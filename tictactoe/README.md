## TicTacToe

[Liam](https://github.com/Gryff) and I were spiking a solution for the tictactoe at the #functionalProgramming session: 

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">At the <a href="https://twitter.com/hashtag/functionalprogramming?src=hash">#functionalprogramming</a> session at <a href="https://twitter.com/codurance">@codurance</a>. This week, <a href="https://twitter.com/hashtag/lazyIO?src=hash">#lazyIO</a>, <a href="https://twitter.com/hashtag/imperativeIO?src=hash">#imperativeIO</a> and <a href="https://twitter.com/hashtag/iterateeIO?src=hash">#iterateeIO</a> facilitated by <a href="https://twitter.com/Parajao">@Parajao</a> <a href="https://t.co/EPNObaMytC">pic.twitter.com/EPNObaMytC</a></p>&mdash; Alvaro Garcia (@alvarobiz) <a href="https://twitter.com/alvarobiz/status/821822514460495877">January 18, 2017</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

This simple session is another iteration of that same problem / practice.

## Development notes

When checking the winning,

  * for rows, using `any` to assert matching the three equal elements
  * for columns:
    * Up to commit 6b4889ef02c6536d8b74b2b79ab5c1c7685ab19a, using a manual check
    * The columns can be checked as the rows if the matrix is transposed (see commit 'Replace algorithm' in 02b397b42f8ee68d12004b258f5a94fdb6585453)

