---
layout: default
title: Math 112 (Autumn 2014)
---

## {{ page.title }}

### Recent announcements
{% for post in site.categories['112aut14'] limit: 3 %}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | remove: '<p>' | remove: '</p>'}}
{% endfor %}
Older announcements are [here](#older-announcements). Homeworks are [here](#homeworks).

----

### Course information

<div class="infotable">

| ------:             | :-----                                                                |
| Time/place:         | MWF 11:30am to 12:20pm in Eck 203                                     |
| Instructor:         | Asilata Bapat (`asilata` at `math` dot `uchicago` dot `edu`)          |
| Office hours:       | M 1:30pm to 2:30pm, W 4:30pm to 5:30pm in [Math-Stat 015][ms]         |
| Abigail's tutorial: | TR 12:00pm to 1:20pm in HGS 108 (`ashearrow` at `uchicago` dot `edu`) |
| Victor's tutorial:  | TR 3:00pm to 4:20pm in E 207 (`zhangvict` at `uchicago` dot `edu`)    |
| Sasha's tutorial:   | TR 4:30pm to 5:50pm in C 203 (`sayvazov` at `uchicago` dot `edu`)     |

[ms]: https://maps.uchicago.edu/?location=Math-Stat+Building


Please also read the [Course Information Sheet](courseinfosheet.pdf) carefully.
</div>

### Syllabus
The class plan is tentative and will be filled in as the quarter goes by. 

<div class="classplan">

| Date  | Topic                                                    |  Section | Homework/Documents        |
| :---  | :---                                                     |     :--- | :---                      |
| 09/29 | Introduction and the triangle game                       |        0 |                           |
| 10/01 | Triangle game, numbers, sets, functions                  |      1.1 |                           |
| 10/03 | Numbers, sets, functions                                 |      1.1 | **[HW 1](#hw-1) due**     |
| 10/06 | Rules of arithmetic                                      | 1.2, 1.3 |                           |
| 10/08 | One's digit arithmetic                                   |      1.4 |                           |
| 10/10 | Axioms in number theory                                  |      2.1 | **[HW 2](#hw-2) due**     |
| 10/13 | Axioms in number theory                                  | 2.1, 2.2 |                           |
| 10/15 | Inequalities and order                                   |      2.2 |                           |
| 10/17 | Inequalities and order                                   |      2.2 | **[HW 3](#hw-3) due**     |
| 10/20 | Divisibility                                             |      3.1 |                           |
| 10/22 | Divisibility and Primes                                  | 3.1, 3.2 |                           |
| 10/24 | **Midterm 1**                                            |          | **[HW 4](#hw-4) due**     |
| 10/27 | Divisibility and Primes                                  | 3.2, 3.3 |                           |
| 10/29 | Primes, Division Algorithm                               | 3.2, 4.1 |                           |
| 10/31 | Division and Euclidean Algorithms                        | 4.1, 4.2 | **[HW 5](#hw-5) due**     |
| 11/03 | The fundamental theorem of arithmetic                    |      4.3 |                           |
| 11/05 | The fundamental theorem of arithmetic, Fibonacci numbers | 4.3, 5.1 |                           |
| 11/07 | Bézout's lemma, number of divisors of a positive integer | 4.3, 5.1 | **[HW 6](#hw-6) due**     |
| 11/10 | $\tau(n)$, $\sigma(n)$                                   |      5.1 |                           |
| 11/12 | $\sigma(n)$, perfect numbers                             |      5.1 |                           |
| 11/14 | **Midterm 2**                                            |          | **[HW 7](#hw-7) due**     |
| 11/17 | Perfect numbers, Mersenne primes                         |      5.1 |                           |
| 11/19 | Perfect numbers, Mersenne primes, LCM (basics)           | 5.1, 5.2 |                           |
| 11/21 | LCM, Egyptian fractions, modular arithmetic              | 5.2, 6.1 | **[HW 7.5](#hw-7.5) due** |
| 11/24 | Modular arithmetic                                       |      6.1 |                           |
| 11/26 | Divisibility tests                                       |      7.1 | **[HW 8](#hw-8) due**     |
| 11/28 | **Thanksgiving vacation**                                |          |                           |
| 12/01 | Project presentations                                    |          |                           |
| 12/03 | Project presentations                                    |          | **Project due**           |
| 12/05 | **Reading Period**                                       |          |                           |
| 12/08 | **Final Examination**                                    |          |                           |

</div>

### Homeworks
Problems marked as "Tutorial" will typically be discussed in tutorial, and are not to be handed in.

#### HW 1
1. Exercise 0.1
1. Exercise 1.4
1. Exercise 1.5
1. Exercise 1.9
1. Can you completely cover a $5\times 5$ chessboard with non-overlapping $1\times 2$ dominoes? Explain why or why not.
1. (Tutorial) Exercise 0.13
1. (Tutorial) Exercise 1.11

#### HW 2
1. 1.12
1. 1.15
1. 1.20
1. 1.25
1. 1.28
1. The numbers 1 through 10 are written in a row. The game is to try and place either $+$ or $-$ signs between consecutive numbers so that the resulting sum is zero. Can you win? (Hint: Use the ideas of Section 1.3)
1. Read section 1.4 on one's digit arithmetic, and answer the following.
    1. What are the square numbers in one's digit arithmetic?
    1. Which of the above numbers have two different square-roots?
1. Solve the following puzzle, such that each letter stands for a different digit between 0 and 9.  
<pre>
  FORTY
    +   TEN
    +   TEN
    -------
    = SIXTY
    </pre>
1. (Tutorial) Consider the set $\{e, a, b\}$. Come up with an abelian group structure on this set such that $e$ is the identity element, and write out the addition table. Is there more than one possible structure? How about on the set $\{e, a, b, c\}$?

#### HW 3
1. Exercise 2.1
1. Exercise 2.7
1. Exercise 2.9
1. Exercise 2.15
1. Exercise 2.16 (See 2.15 for the definition of the dictionary order.)
1. Exercise 2.18
1. What is the last digit of the number $3^{2014}$? (Challenge: What are the last *two* digits of the number $3^{2014}$?)
1. (Tutorial) Practice Problem 2.5
1. (Tutorial) Exercise 2.3

#### HW 4
In this homework, you will read some "light" articles related to mathematics. The readings are up on the Chalk page under Assignments folder. Each reading assignment has an associated short writing assignment, also up on Chalk.

#### Midterm 1 Review
1. Review the triangle game, especially all practice problems. Review the cross game from the homework. What are the symmetries in each case? What is the reasoning for coming up with the solutions?
1. Read and remember all the axioms of addition, multiplication, and ordering.
1. What is the additive inverse of $1$ in $\mathbb{Z}$? In even-odd arithmetic? In one's digit arithmetic?
1. What is the multiplicative inverse of $1$ in each of the above systems? (Does it always exist?) Which elements have multiplicative inverses in each of the above systems?
1. Review the proofs of the theorems from the book, such as $0\cdot a = 0$, etc. What axioms do we use in each case?
1. Think about whether it is possible to give an order on the set $\{0, 1\}$, such that the four axioms O1, O2, O3, O4 hold for even/odd arithmetic. If yes, what are the possiblities? If not, why not?

#### HW 5
1. Exercise 3.1
1. Exercise 3.7 (a, c, h, j)
1. Exercise 3.9
1. Exercise 3.12
1. Exercise 3.13
1. Show that if $x$ and $y$ are integers, and the number $(15x + 2y)$ is divisible by $11$, then the number $(4x + 13y)$ must also divisible by 11.

#### HW 6
1. Exercise 3.15 (You don't have to write up the "Think about..." part of this problem, but do think about it!)
1. Exercise 4.3 (b, e)
1. Exercise 4.6
1. Exercise 4.10 (Read through Exercise 4.9 first. You don't need to write up the "Then, explain how..." part of this problem, but do think about it!)
1. Exercise 4.14
1. Exercise 4.17
1. Challenge: Show that for every $n$, we have $(f_{n+1},f_{n+2}) = (f_{n-1}, f_n)$, where the $f$s are the Fibonacci numbers.

#### HW 7
1. Exercise 5.4
1. Exercise 5.5
1. Exercise 5.8
1. Exercise 5.9
1. Exercise 5.13

#### Midterm 2 Review
1. Review the proofs of theorems 3.1 to 3.7.
1. Review the proof of theorem 3.10.
1. Review definitions: What are primes, twin primes? When are two numbers relatively prime?
1. Read the division and Euclidean algorithms and practice some examples.
1. Review the fundamental theorem of arithmetic. Do some practice problems, such as 4.19 and 4.21.
1. Read the definition of the Fibonacci numbers.
1. How do you compute the number of divisors of a number $n$? Do several examples. Remember that $\tau(n)$ is odd if and only if $n$ is a perfect square.
1. How can you describe all numbers $n$ for which $\tau(n) = 5$?


#### HW 7.5
This homework is due by email, and it is due by 5pm on Friday, Nov 21. This homework will be graded for completion, not for content.

1. Email me your project drafts _as they are in their current state_. One person from each team should send the email and cc your teammates (though of course you should have worked on the drafts collectively). Don't worry much about errors, gaps, or formatting.
1. Send me the following information individually in a short email (don't cc your teammates):
    * What has your individual contribution been to the project write-up so far?
    * What more do you individually plan to put in the project write-up?

#### HW 8
1. Exercise 5.17
1. Exercise 5.18
1. Exercise 5.29 (Hint: Multiply both sides of the equation by $n$.)
1. Exercise 6.1
1. Exercise 6.4
1. Exercise 6.16
1. (Tutorial) Exercise 6.17
1. (Tutorial) Exercise 5.22

#### Final Exam Review
1. The syllabus for the midterm consists of everything that we have done since the beginning of the course. The new material consists of chapter 5 and parts of chapter 6 and 7: most of section 6.1 (congruences, addition and multiplication under congruences, and what axioms they satisfy), and section 7.1 (divisibility tests).
1. (Important) Go over all old homeworks and review guidelines for Midterms 1 and 2. 
1. Review how to compute $\sigma(n)$.
1. Review the definition of perfect numbers, and of Mersenne numbers.
1. Review the proof of theorem 5.7.
1. Review LCM and the formula $a\times b = \text{gcd}(a,b)\times \text{lcm}(a,b)$.
1. Review how to compute the Egyptian fraction decomposition.
1. Go over the definition of congruence modulo some number, and of a residue class or congruence class (on page 131).
1. Review how to add, subtract, and multiply congruence classes (Theorem 6.3 , 6.4).
1. Review all the divisibility tests we learned in class. You may be asked some indirect problems from this section, so make sure you understand how the tests work, especially the combined test for 7, 11, and 13.

----

### Older announcements
{% for post in site.categories['112aut14'] offset: 3%}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | remove: '<p>' | remove: '</p>' }}
{% endfor %}
