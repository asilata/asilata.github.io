---
layout: default
title: Math 151 (Autumn 2013)
---

## {{ page.title }}

### Recent announcements
{% for post in site.categories['151aut13'] limit: 3 %}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | strip_html}}
{% endfor %}
Older announcements are [here](#older-announcements).

----

### Course information
<div class="infotable">

| ---------------:     | :-----------------------------------------------------------                                          |
| Time/place:          | Tuesday/Thursday, 10:30am to 11:50am in SS 108                                                        |
| Instructor:          | Asilata Bapat (`asilata` at `math` dot `uchicago` dot `edu`)                                          |
| Office hours:        | Monday 4pm to 5pm, Wednesday 4pm to 5:30pm, and any other time by appointment, in [Math/Stat][ms] 015 |
| Problem session:     | Tuesday 6pm to 7pm in Eckhart 308                                                                     |
| VCA:                 | Mike Fosco (`mfosco` at `uchicago` dot `edu`)                                                         |
| Mike's office hours: | Monday 6:30pm to 7:30pm in Harper cafe                                                                |

[ms]: https://maps.uchicago.edu/?location=Math-Stat+Building

</div>

Additionally, please read the [Course Information Sheet](courseinformationsheet.pdf) carefully.

### Syllabus
The class plan is tentative and will be updated as the quarter goes by. You will get more out of each class if you read the relevant sections of the textbook beforehand.

<div class="classplan">

| Date  | Topic                                                   | Section       | Homework/Documents                                        |
| :---- | :-----------------------------                          | :---------    | :-------------------                                      |
| 10/01 | Review, inequalities                                    | 1.3, 1.6      | [Diagnostic test](diagnostictest.pdf)                     |
| 10/03 | Review, proofs and induction                            | 1.6, 1.8      | **[HW 0](#hw-0) due**                                     |
| 10/08 | Induction, introduction to limits, definition           | 1.8, 2.1, 2.2 |                                                           |
| 10/10 | Definition of limits, some limit theorems               | 2.2, 2.3      | **[HW 1](#hw-1) due**                                     |
| 10/15 | Limit theorems, pinching theorem                        | 2.3, 2.5      |                                                           |
| 10/17 | Pinching theorem, review                                | 2.5           | **[HW 2](#hw-2) due**                                     |
| 10/22 | **Midterm 1**                                           |               | [Practice problems][p1], [Solutions][s1], [Histogram][h1] |
| 10/24 | Trigonometric limits, continuity                        | 2.5, 2.4      |                                                           |
| 10/29 | Intermediate and extreme value theorems, the derivative | 2.6, 3.1      | **[HW 3](#hw-3) due**                                     |
| 10/31 | Differentiation formulas, rates of change               | 3.2, 3.3, 3.4 |                                                           |
| 11/05 | The chain rule, differentiation trigonometric functions | 3.5, 3.6      | **[HW 4](#hw-4) due**                                     |
| 11/07 | Implicit differentiation, rational powers               | 3.7           |                                                           |
| 11/12 | **Midterm 2**                                           |               | [Practice problems][p2], [Solutions][s2], [Histogram][h2] |
| 11/14 | Rolle's theorem, mean-value theorem                     | 4.1           | **[HW 5](#hw-5) due**                                     |
| 11/19 | Increasing and decreasing functions, extreme values     | 4.2, 4.3      |                                                           |
| 11/21 | Extreme values, max-min problems                        | 4.3, 4.4, 4.5 | **[HW 6](#hw-6) due**                                     |
| 11/26 | Max-min problems, concavity, points of inflection       | 4.5, 4.6      |                                                           |
| 11/28 | **Thanksgiving Vacation**                               |               |                                                           |
| 12/03 | Velocity, acceleration, speed, and catchup              | 4.9           | **[HW 7](#hw-7) due**                                     |
| 12/05 | **Reading Period**                                      |               |                                                           |
| 12/10 | **Final Examination**                                   |               | [Practice problems][pf]                                   |

[p1]: #midterm-1-practice-problems
[s1]: midterm1-solutions.pdf
[h1]: Midterm1Graph.pdf
[p2]: #midterm-2-practice-problems
[s2]: midterm2-solutions.pdf
[h2]: Midterm2Graph.pdf
[pf]: #final-exam-practice-problems

</div>

### Homework
Homework will be due _before class_ on the day indicated on the syllabus. Late homework will not be accepted. The lowest percentage homework score will be dropped.

#### HW 0
This homework will not count towards your final grade, but please turn it in anyway. Numbered problems are from the textbook.

* Solve 1.3.1, 1.3.2, 1.3.6, 1.3.14, 1.3.32

#### HW 1

Only the problems marked "turn in" are to be turned in.

* **Turn in:** 1.6 (1, 3, 6, 10, 64, 67), 1.8 (6, 8, 10), 2.1 (2, 4), 2.2 (21, 22, 24, 26)
* **Read:** [_How to write proofs_](proofguide.pdf) by Eugenia Cheng.
* **Practice:** 1.3 (14, 16, 28, 44, 53), 1.6 (11, 12)
* **Challenge:** Suppose $a$, $b$, $c$ are real numbers such that $(a+b+c)$, $(ab+bc+ca)$ and $abc$ are all integers. Prove that for every $n\in \mathbb{N}$, the number $(a^n + b^n + c^n)$ is an integer.

#### HW 2

* **Turn in:** 2.2 (41 without writing justifications, 42, 50, 52, 54), 2.3 (8, 14, 16, 22, 26, 45--50 without writing justifications)
* **Practice:** 2.2 (36, 38, 40), 2.3 (1, 2, 39, 40, 52, 53)
* **Challenge:** 2.2 (46, 56), 2.3 (56)

#### Midterm 1 Practice Problems
* 1.3 (4, 8, 30, 43--46)
* 1.5 (7--10, 49--54)
* 1.8 (11, 17)
* 2.1 (1--10)
* 2.2 (43, 44, 49, 53)
* 2.3 (7, 9, 10, 32, 36, 37, 43, 44)

#### HW 3
* **Read:** The definitions of removable and essential (jump or infinite) discontinuities on pages 82 and 83.
* **Turn in:** 2.5 (5, 6, 12, 18, 24, 31, 33, 34, 38, 40, 43, 47), 2.4 (1, 4, 8)
* **Practice:** 2.5 (1, 2, 8, 11, 14, 37), 2.4 (2, 3, 5, 14)
* **Challenge:** 2.5 (46, 49, 50)

#### HW 4
* **Turn in:** 2.4 (45--47, 52), 2.6 (2, 6, 9, 25, 26, 29), 3.2 (32, 36, 49), 3.3 (16, 40, 53), 3.4 (5, 6)
* **Practice:** 2.4 (13--24), 2.6 (27, 30, 31, 32), 3.1 (43--48, 58, 59)
* **Challenge:** 2.4 (50, 53)

#### Midterm 2 Practice Problems
* 2.4 (29, 30, 35, 36, 37, 54)
* 2.5 (some selection from 1--37)
* 2.6 (1, 10, 11, 13--24, 35)
* 3.1 (3, 6, 7, 23, 28, 30, 32, 33--38, 41, 42, 52)
* 3.2 (some selection from 1--20, 24, 28, 29, 50, 52, 56, 59)
* 3.4 (8, 12, 13, 15, 16)
* 3.5 (see HW 5, and also 60, 62, 66)
* 3.6 (see HW 5, and also 71, 72)

#### HW 5
* **Turn in:** 3.5 (16, 28, 48, 59), 3.6 (6, 9, 58), 3.7 (6, 10, 55)

#### HW 6
* **Read:** Theorem 4.1.2 and the proof, and Rolle's theorem and the proof.
* **Turn in:** 3.7 (38, 42, 49, 50, 53), 4.1 (12, 15, 16, 24, 26), 4.2 (1, 12)
* **Practice:** 4.1 (2, 3, 7, 28, 35), 4.2 (37--40)

#### HW 7
* **Turn in:** 4.3 (3, 12, 31, 35), 4.4 (26, 36, 41, 42, 48), 4.5 (4, 18, 24, 33), 4.6 (18, 40)

#### Final Exam Practice Problems
These are problems from Chapter 4. For practice problems on the earlier material please refer to the practice problems for the midterm, and practice similar problems from the textbook or outside.

* 4.1 (4, 8, 19, 29, 38)
* 4.2 (2, 4, 20, 47--50)
* 4.3 (6, 8, 33, 40, 43)
* 4.4 (1, 2, 4, 24, 47)
* 4.5 (5, 8, 21, 28, 38, 46, 49)
* 4.6 (6, 10, 15)
* 4.9 (11--20, 40, 53, 59)

----

### Older announcements
{% for post in site.categories['151aut13'] offset: 3%}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | strip_html }}
{% endfor %}

