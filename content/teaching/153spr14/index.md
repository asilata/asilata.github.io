---
layout: default
title: Math 153 (Spring 2014)
---

## {{ page.title }}

### Recent announcements
{% for post in site.categories['153spr14'] limit: 3 %}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | strip_html}}
{% endfor %}
Older announcements are [here](#older-announcements).

----

### Course information
<div class="infotable">

| ---------------:     | :-----------------------------------------------------------                                                   |
| Time/place:          | Tuesday/Thursday, 10:30am to 11:50am in SS 108                                                                 |
| Instructor:          | Asilata Bapat (`asilata` at `math` dot `uchicago` dot `edu`)                                                   |
| Office hours:        | Mondays 12:30pm--1:30pm, Wednesdays 4pm--5pm and by appointment on Tuesdays 5pm--6pm or Fridays 4:30pm--5:30pm |
| Problem session:     | Tuesdays 6pm--7pm in Eck 308                                                                                   |
| VCA:                 | Mike Fosco (`mfosco` at `uchicago` dot `edu`)                                                                  |
| Mike's office hours: | Tuesday 3pm--4pm in Stuart Cafe                                                                                |

</div>

Additionally, please read the [Course Information Sheet](courseinformationsheet.pdf) carefully.

### Syllabus
The class plan is tentative and will be updated as the quarter goes by. You will get more out of each class if you read the relevant sections of the textbook beforehand.

<div class="classplan">

| Date  | Topic                                                   | Section       | Homework/Documents          |
| :---  | :---                                                    | :---          | :---                        |
| 04/01 | Inverse trig functions, hyperbolic sine and cosine      | 7.7, 7.8      |                             |
| 04/03 | Integration by parts, powers/products of trig functions | 8.2, 8.3      |                             |
| 04/08 | Integration techniques, partial fractions               | 8.3, 8.4, 8.5 |                             |
| 04/10 | Integration techniques, partial fractions               | 8.4, 8.5      | **[HW 1](#hw-1) due**       |
| 04/15 | Some differential equations, catchup                    | 9.1, 9.2      |                             |
| 04/17 | **Quiz 1**                                              |               | **[Formula sheet][f1]**     |
| 04/22 | The least-upper-bound principle, sequences, examples    | 11.1, 11.2    |                             |
| 04/24 | The limit of a sequence, examples                       | 11.3          | **[HW 2](#hw-2) due**       |
| 04/29 | Some important limits                                   | 11.3, 11.4    |                             |
| 05/01 | Indeterminate forms                                     | 11.5, 11.6    | **[HW 3](#hw-3) due**       |
| 05/06 | **Quiz 2**                                              |               | **[Practice problems][p2]** |
| 05/08 | Improper integrals                                      | 11.7          |                             |
| 05/13 | Infinite series: introduction and examples              | 12.1, 12.2    |                             |
| 05/15 | Convergence tests                                       | 12.3, 12.4    | **[HW 4](#hw-4) due**       |
| 05/20 | Convergence tests, conditional convergence              | 12.4, 12.5    |                             |
| 05/22 | **Quiz 3**                                              |               | **[Practice problems][p3]** |
| 05/27 | Taylor polynomials and Taylor series                    | 12.6, 12.7    | **[HW 5](#hw-5) due**       |
| 05/29 | Taylor series and power series                          | 12.7, 12.8    |                             |
| 06/03 | Differentiation/integration of power series, catchup    | 12.9          | **[HW 6](#hw-6) due**       |
| 06/05 | **Reading Period**                                      |               | **[Practice problems][pf]** |
| 06/12 | **Final Examination**                                   |               | **[Formula sheet][ff]**     |

[f1]: quiz1formulasheet.pdf
[p2]: #quiz-2-practice-problems
[p3]: #quiz-3-practice-problems
[pf]: #final-exam-practice-problems
[ff]: final-formulasheet.pdf

</div>

### Homeworks

#### HW 1
* 7.7 (1, 9, 14, 22, 26, 35)
* 7.8 (4, 20, 26, 32, 52)
* 8.2 (2, 12, 28, 38, 44)
* 8.3 (5, 6, 27, 35)

#### HW 2
* 8.4 (33, 36, 44)
* 8.5 (6, 7, 21, 24, 35, 45)
* 9.1 (4, 9, 24, 29)
* 9.2 (7, 14)

#### HW 3
* **Turn in:**
    * 9.2 (15, 20, 21)
    * 11.1 (no justification needed: 3, 10)
    * 11.2 (12, 22, 39)
    * 11.3 (9, 28, 40)

* **Practice only:** (Discuss these with other people if possible.)
    * 9.2 (24, 28)
    * 11.1 (25, 28)
    * 11.2 (43)

#### Quiz 2 Practice Problems
* 8.5 (10, 18, 38)
* 9.1 (19, 23, 37)
* 9.2 (2, 19, 23)
* 11.2 (28, 32, 41)
* 11.3 (25, 30, 36, 47)

#### HW 4
* 11.5 (10, 20, 23, 46, 53)
* 11.6 (3, 8, 13, 29, 34)
* 11.7 (4, 11, 20, 25, 33)

#### Quiz 3 Practice Problems
* 11.4 (4, 6, 23, 26)
* 11.5 (9, 34, 54, see HW 4)
* 11.6 (1, 24, 37, 60, see HW 4)
* 11.7 (15, 16, 31, 34, see HW 4)
* 12.2 (3, 10, 23, 29)
* 12.3 (1, 20, 31, 37, 47, 48)

#### HW 5
* **Read:** Sections 12.3 and 12.4 carefully.
* 12.3 (12, 19)
* 12.4 (14, 21, 31)

#### HW 6
* 12.5 (16, 24)
* 12.6 (6, 9)
* 12.7 (1, 8, 25)
* 12.8 (2, 14, 42)

#### Final Exam Practice Problems
* For earlier sections, refer to previous homeworks, quizzes and practice problems.
* 12.5 (5, 27)
* 12.6 (15, 36, 51)
* 12.7 (1, 7, 10)
* 12.8 (5, 11, 12, 20, 30)
* 12.9 (1, 4, 7, 28). The main hint for these problems is that power series can be integrated or differentiated term-by-term in any open interval where they converge.

----

### Older announcements
{% for post in site.categories['153spr14'] offset: 3%}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | strip_html }}
{% endfor %}



