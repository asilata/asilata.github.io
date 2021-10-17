---
layout: default
analytics: false
title: Math 162 (Winter 2016)
---

## {{ page.title }}

### Recent announcements
{% for post in site.categories['162win16'] limit: 3 %}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | remove: '<p>' | remove: '</p>'}}
{% endfor %}
Older announcements are [here](#older-announcements). Course materials are [here](#course-materials). Problem sets are [here](#problem-sets).

----

### Course information

<div class="infotable">

| ------:         | :-----                                                                                                                                                                                                                                                                                                    |
| Time/place:     | TuTh 10:30am to 11:50pm in [Ryerson][ry] 358                                                                                                                                                                                                                                                              |
| Instructors:    | Asilata Bapat (`asilata` at `math` dot `uchicago` dot `edu`), office hours Tu 17:00--18:00, Th 9:00--10:00 and by appointment in [Eckhart][eck] 2A<br/>Matthew Creek (`mcreek` at `math` dot `uchicago` dot `edu`), office hours Tu 9:00--10:00, W 16:00--17:00, and by appointment in [Eckhart][eck] 306 |
| College fellow: | Anthony Wang (`ayw` at `uchicago` dot `edu`), office hours M 16:00--17:00, F 16:00--17:00, and by appointment in [Woodlawn][wood] 5E, problem session M 18:00--19:00 in [Ryerson][ry] 358                                                                                                                 |


[eck]: https://maps.uchicago.edu/?location=Eckhart+Hall
[wood]: https://maps.uchicago.edu/?location=5720+South+Woodlawn+Avenue
[ry]: https://maps.uchicago.edu/?location=Ryerson+Laboratory

</div>

### Class plan

<div class="classplan">

|-------+--------------------------------------------------------+---------------------------------------------------------------------------------+-----------------------------------|
| Date  | Topic                                                  | Preparation Assignment                                                          | Additional Information            |
| :---  | :---                                                   | :---                                                                            | :---                              |
| 01/05 | Compactness                                            | Script #4, up to and including Theorem 4.26                                     |                                   |
| 01/07 | Compactness, Heine-Borel                               | Script #4, up to and including Theorem 4.36 (through the end)                   |                                   |
| 01/12 | Heine-Borel, Bolzano-Weierstrass, Continuous functions | Script #4 (through the end) and Script #5, up to and including Proposition 5.11 |                                   |
| 01/14 | Subspace topology, continuous functions                | Script #5, up to and including Theorem 5.17                                     |                                   |
| 01/19 | Continuous functions, intermediate value theorem       | Script #5, up to and including Exercise 5.21 (through the end)                  | Journal due in class (Script #4)  |
| 01/21 | Extreme value theorem, field axioms                    | Script #6, up to and including Lemma 6.18                                       | Problem set #1 due in class       |
| 01/26 | Ordered fields                                         | Script #6, up to and including Theorem 6.27 (through the end)                   |                                   |
| 01/28 | Ordered fields                                         | Script #7, up to and including Corollary 7.14                                   | Journal due in class (Script #5)  |
| 02/02 | Dedekind cuts                                          | Script #7, up to and including Corollary 7.14                                   |                                   |
| 02/04 | Dedekind cuts                                          | Script #7, up to and including Corollary 7.14                                   | Journal due in class (Script #6)  |
| 02/09 | **Anthony's lecture** (more about $$\mathbb{R}$$)      |                                                                                 | Read [supplementary document][of] |
| 02/11 | Properties of the reals                                | Script #7, up to and including Theorem 7.22                                     | Problem set #2 due in class       |
| 02/16 | Sequences and limits                                   | Script #8, up to and including Lemma 8.17                                       |                                   |
| 02/18 | Sequences and limits                                   | Script #8, up to and including Theorem 8.22 (through the end)                   | Problem Set #3 due in class       |
| 02/23 | Sequences and limits                                   | Script #9, up to and including Theorem 9.5                                      | Journal due in class (Script #7)  |
| 02/25 | Limits of functions                                    | Script #9, up to and including Corollary 9.14                                   | Problem Set #4 due in class       |
| 03/01 | Limits of functions                                    | Script #9, up to and including Theorem 9.20                                     | Journal due in class (Script #8)  |
| 03/03 | Limits of functions, derivatives                       | Script #9, up to and including Theorem 9.27                                     |                                   |
| 03/08 | Derivatives, mean-value theorem                        | Script #9, up to and including Corollary 9.37 (through the end)                 |                                   |
| 03/10 | **Reading period**                                     |                                                                                 |                                   |
| 03/15 |                                                        |                                                                                 |                                   |
| 03/17 | **Written final examination**                          |                                                                                 |                                   |
|-------+--------------------------------------------------------+---------------------------------------------------------------------------------+-----------------------------------|

</div>

### Course materials

* [Course information sheet](documents/courseinfosheet.pdf)
* [IBL primer](documents/ibl.pdf)
* [Script #0](scripts/script_0_161.pdf) ([source code](scripts/script_0_161.tex))
* [Script #1](scripts/script_1_161.pdf) ([source code](scripts/script_1_161.tex))
* [Script #2](scripts/script_2_161.pdf) ([source code](scripts/script_2_161.tex))
* [Script #3 original](scripts/script_3_161_original.pdf) ([source code](scripts/script_3_161_original.tex))
* [Script #3 updated](scripts/script_3_161.pdf) ([source code](scripts/script_3_161.tex))
* [Script #4](scripts/script_4_161.pdf) ([source code](scripts/script_4_161.tex))
* [Script #5](scripts/script_5_161.pdf) ([source code](scripts/script_5_161.tex))
* [Script #6](scripts/script_6_161.pdf) ([source code](scripts/script_6_161.tex))
* [Script #7](scripts/script_7_161.pdf) ([source code](scripts/script_7_161.tex))
* [Supplementary document][of] about a function from one ordered set to another
* [Notes from Anthony's lecture](scripts/real-properties.pdf)
* [Script #8](scripts/script_8_161.pdf) ([source code](scripts/script_8_161.tex))
* [Script #9](scripts/script_9_161.pdf) ([source code](scripts/script_9_161.tex))

[of]: scripts/order-function.pdf


### Problem sets

* [Problem Set 1](problem_sets/ps1.pdf): Due on Thursday, Jan 21
* [Problem Set 2](problem_sets/ps2.pdf): Due on Thursday, Feb 11
* [Problem Set 3](problem_sets/ps3.pdf): Due on Thursday, Feb 18
* [Problem Set 4](problem_sets/ps4.pdf): Due on Thursday, Feb 25

----

### Older announcements
{% for post in site.categories['162win16'] offset: 3%}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | remove: '<p>' | remove: '</p>' }}
{% endfor %}
