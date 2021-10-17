---
layout: default
title: Math 153 (Spring 2013)
---

## {{ page.title }}

### Recent announcements
{% for post in site.categories['153spr13'] limit: 3 %}
* _({{ post.date | date: "%a %b %d" }})_ {{ post.content | strip_html }}
{% endfor %}
Older announcements are [here](#older-announcements).

----

### Course information
**Time/place:** Tuesday/Thursday, 9am to 10:30am in Eck 203.  
**Instructor:** Asilata Bapat (`asilata` at `math` dot `uchicago` dot `edu`)  
**Office:** Math/Stat 015 (5727 S University Ave)  
**Office hours:** Tuesday 3pm to 4pm, Wednesday 5pm to 6pm, Thursday 1pm to 2pm, and any other time by appointment.  
**VCA:** Yo Joong Choe  
**YJ's office hours:** Wednesday 3:30pm to 4:30pm in the C-Shop, Reynolds Club.  
**Problem session:** Tuesday 5pm to 6pm in the Barn (Ryerson 352).  
**Textbook:** _Calculus_ by Salas, Hille, Etgen, 10th edition.  

Grades will be based on homework, two midterms and a final exam. The grade distribution for the class will be as follows.

* Homework: 10%
* Higher-scoring midterm: 30%
* Lower-scoring midterm: 20%
* Final examination: 40%

Please also read the Course Information Sheet ([PDF](courseinformation.pdf)) carefully.

### Class plan and homework
Homework will be due _before class_ on Thursday each week. Late homework will not be accepted. The lowest homework score will be dropped.

The class plan is tentative and will be updated as the quarter goes by. You will get more out of each class if you read the relevant sections of the textbook beforehand.

<div class="classplan">

|--------+---------------------------------------------------------------------------------+------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Date   | Topic                                                                           | Section          | Homework/Documents                                                                                                                                                                                                                                    |
| :---   | :-----                                                                          | :------          | :------                                                                                                                                                                                                                                               |
| Apr 05 | Hyperbolic sine and cosine                                                      | 7.8              | HW 1: Ch 7 review exercises (7, 13, 14, 24, 31, 52) and 7.7 (16, 25, 31, 35)                                                                                                                                                                          |
| Apr 09 | Integration by parts, powers/products of trig functions                         | 8.2, 8.3         |                                                                                                                                                                                                                                                       |
| Apr 11 | Integrating $\sqrt{a^2 - x^2}$ etc and rational functions                       | 8.4, 8.5         | HW 2: 7.7 (43, 64, 71, 73), 7.8 (3, 8, 10, 13, 20, 26, 29, 34, 43, 44), 8.2 (1, 4, 6, 17) and 8.3 (1, 5)                                                                                                                                              |
| Apr 16 | Integrating partial fractions                                                   | 8.5              | [Formula Sheet (PDF)](midterm1formulasheet.pdf) for Midterm 1                                                                                                                                                                                         |
| Apr 18 | Some differential equations, Catchup                                            | 9.1, 9.2         | HW 3: 8.2 (38, 45, 77), 8.3 (7, 8, 11, 36), 8.4 (8, 11, 26, 33, 44), 8.5 (1, 5, 9)                                                                                                                                                                    |
| Apr 23 | _Midterm 1_                                                                     |                  | Solutions (removed) and [Score Histogram (PDF)](Midterm1Graph.pdf)                                                                                                                                                                                    |
| Apr 25 | The least-upper-bound axiom, sequences of real numbers                          | 11.1, 11.2       | HW 4: Read sections 9.1 and 9.2. Solve 9.1 (10, 26, 29) and 9.2 (8, 16)                                                                                                                                                                               |
| Apr 30 | The limit of a sequence                                                         | 11.3             |                                                                                                                                                                                                                                                       |
| May 02 | The limit of a sequence, some important limits                                  | 11.3, 11.4       | HW 5: Read the [_How to write proofs_](proofguide.pdf) guide by Eugenia Cheng. Review the principle of mathematical induction. Solve 11.1 (6, 8, 16, 22, 24, 30: no explanations needed), 11.2 (16, 22, 45, 48, 60, 62, 64), 11.3 (8, 10, 18, 21, 32) |
| May 07 | Indeterminate forms                                                             | 11.5, 11.6       |                                                                                                                                                                                                                                                       |
| May 09 | Improper integrals                                                              | 11.7             | HW 6: Read sections 11.3 and 11.4 carefully. Solve 11.3 (51, 52, 56, 58), 11.4 (2, 6, 12, 14, 16, 23), 11.5 (1, 2, 6, 14, 24)                                                                                                                         |
| May 14 | Improper integrals, Sigma notation                                              | 11.7, 12.1       | [Review sheet (PDF)](midterm2reviewsheet.pdf) for Midterm 2                                                                                                                                                                                           |
| May 16 | Infinite series, tests for convergence                                          | 12.2, 12.3       | HW 7: 11.5 (32, 47, 49), 11.6 (1, 8, 26, 31, 42), 11.7 (1, 2, 7, 17, 22), 12.1 (3, 6)                                                                                                                                                                 |
| May 21 | _Midterm 2_                                                                     |                  |                                                                                                                                                                                                                                                       |
| May 23 | Tests for convergence, absolute and conditional convergence, alternating series | 12.3, 12.4, 12.5 |                                                                                                                                                                                                                                                       |
| May 28 | Taylor polynomials and Taylor series                                            | 12.6, 12.7       |                                                                                                                                                                                                                                                       |
| May 30 | Taylor polynomials and Taylor series, power series                              | 12.7, 12.8       |                                                                                                                                                                                                                                                       |
| Jun 04 | Differentiation and integration of power series                                 | 12.9             | HW 8: 12.2 (2, 7, 8, 29), 12.3 (6, 9, 12, 17, 39(a), 39(b)), 12.4 (1, 9, 16, 28, 34, 43), 12.5 (1, 2), 12.6 (11, 13), [Solutions (PDF)](hw8solutions.pdf)                                                                                             |
| Jun 06 | _Reading Period_                                                                |                  | [Review sheet (PDF)](finalreviewsheet.pdf) for Final Exam, Solutions for Midterm 2 (removed), [Solutions (PDF)](finalpsetsolutions.pdf) for final problem set, [Formula sheet (PDF)](finalformulasheet.pdf) for Final Exam                            |
| Jun 11 | _Final Examination_                                                             |                  |                                                                                                                                                                                                                                                       |
|--------+---------------------------------------------------------------------------------+------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|


### Older announcements
{% for post in site.categories['153spr13'] offset: 3%}
* _({{ post.date | date: "%a %b %d" }})_ {{ post.content | strip_html }}
{% endfor %}
