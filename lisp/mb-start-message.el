(defvar *code-quotes* '("Most of the biggest problems in software are problems of misconception. - Rich Hickey"
                        "Constraints are advantages in disguise. - 37 Signals"
                        "Don't have good ideas if you aren't willing to be responsible for them. - Alan Perlis"
                        "Fools ignore complexity. Pragmatists suffer it. Some can avoid it. Geniuses remove it. - Alan Perlis"
                        "The most important property of a program is whether it accomplishes the intention of its user. - C.A.R. Hoare"
                        "Inside every large program, there is a small program trying to get out. - C.A.R. Hoare"
                        "Organizations which design systems are constrained to produce designs which are copies of the communication structures of these organizations. - Conway's Law"
                        "Just because something is easy to measure doesn't mean it's important. - D.H.H."
                        "Duplication is far cheaper than the wrong abstraction. - Sandi Metz"
                        "A program is like a poem: you cannot write a poem without writing it. - E.W. Dijkstra"
                        "Simplicity is prerequisite for reliability. - E.W. Dijkstra"
                        "Good programmers don't just write programs. They build a working vocabulary. - Guy Steele"
                        "Machines should work. People should think. - IBM Pollyanna Principle"
                        "Good software, like wine, takes time. - Joel Spolsky"
                        "Without great solitude no serious work is possible - Pablo Picasso"
                        "No code is faster than no code. - Merb Motto"
                        "A user interface should be so simple that a beginner in an emergency can understand it within ten seconds. - Ted Nelson"
                        "Make it work first before you make it work fast. - Bruce Whiteside"
                        "If you aren't sure which way to do something, do it both ways and see which works better. - John Carmack"
                        "Premature optimizations can be troublesome to revert, but premature generalizations are often near impossible. - Emil Persson"
                        "In programming, the hard part isn't solving problems, but deciding what problems to solve. - Paul Graham"
                        "Theory and practice sometimes clash. And when that happens, theory loses. Every single time. - Linus Torvalds"
                        "If we want users to like our software we should design it to behave like a likeable person: respectful, generous and helpful. - Alan Cooper"
                        "The lurking suspicion that something could be simplified is the world's richest source of rewarding challenges. - Edsger Dijkstra"
                        "The computing scientist's main challenge is not to get confused by the complexities of his own making. - E. W. Dijkstra"
                        "If testing seems hard, there's something wrong with your design. - Sandi Metz"
                        "Simple things should be simple, complex things should be possible. - Alan Kay"
                        "Simplicity and elegance are unpopular because they require hard work and discipline to achieve and education to be appreciated. - E.W. Dijkstra"
                        "Any fool can write code that a computer can understand.  Good programmers write code that humans can understand. - Martin Fowler"
                        "The purpose of abstraction is not to be vague, but to create a new semantic level in which one can be absolutely precise - Edsger Dijkstra"
                        "Programming is an art form that fights back. - Unknown"
                        "The computer programmer is a creator of universes for which he alone is responsible. - Joseph Weizenbaum"
                        "The effective exploitation of his powers of abstraction [is] one of the most vital activities of a competent programmer. - Edsger Dijkstra"
                        "No matter how slow you are writing clean code, you will always be slower if you make a mess. - Uncle Bob Martin"
                        "Good judgement is the result of experience. Experience is the result of bad judgement. - Fred Brooks"
                        "Anything that can possibly go wrong, will go wrong. - Murphy's Law"
                        "It always takes longer than you expect, even when you take into account Hofstadter's Law. — Hofstadter's Law"
                        "A clever person solves a problem. A wise person avoids it. — Albert Einstein"
                        "One accurate measurement is worth more than a thousand expert opinions. - Admiral Grace Hopper"))

(defun mb/fortune ()
  (nth (random (length *code-quotes*)) *code-quotes*))

(setq-default initial-scratch-message
              (concat ";; " (mb/fortune) "\n\n"))


(provide 'mb-start-message)
