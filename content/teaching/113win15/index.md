---
layout: default
title: Math 113 (Winter 2015)
---


## {{ page.title }}

### Recent announcements
{% for post in site.categories['113win15'] limit: 3 %}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | remove: '<p>' | remove: '</p>'}}
{% endfor %}
Older announcements are [here](#older-announcements). Homeworks are [here](#homeworks).

----

### Course information

<div class="infotable">

| ------:       | :-----                                                       |
| Time/place:   | MWF 11:30am to 12:20pm in Kent 106                           |
| Instructor:   | Asilata Bapat (`asilata` at `math` dot `uchicago` dot `edu`) |
| Office hours: | W 2:30pm to 3:30pm and Th 3pm to 4pm                         |
| Tutor:        | Sasha Ayvazov (`sayvazov` at `uchicago` dot `edu`)           |

[ms]: https://maps.uchicago.edu/?location=Math-Stat+Building

</div>

### Syllabus
The class plan is tentative and will be filled in as the quarter goes by. 

<div class="classplan">

| Date  | Topic                                     | Section          | Homework/Documents    |
| :---  | :---                                      | :---             | :---                  |
| 01/05 | Introduction to Geometry                  | 9                |                       |
| 01/07 | Symmetry group (equilateral triangle)     | 11.1             |                       |
| 01/09 | Symmetry group (triangle and square)      | 11.1,11.2        | **[HW 1](#hw-1) due** |
| 01/12 | Basics of Euclidean geometry              | 10.1, Appendix A | [Notes][eg]           |
| 01/14 | Ruler-compass constructions, circumcircle |                  |                       |
| 01/16 | Orthocenter, incenter, centroid           |                  | **[HW 2](#hw-2) due** |
| 01/19 | **Martin Luther King, Jr. Day**           |                  |                       |
| 01/21 | Ceva's theorem and applications           |                  |                       |
| 01/23 | The nine-point circle                     |                  | **[HW 3](#hw-3) due** |
| 01/26 | The symmetric group                       | 12.1             |                       |
| 01/28 | The symmetric group                       | 12.2             |                       |
| 01/30 | **Midterm 1**                             |                  |                       |
| 02/02 | The symmetric group                       | 12.3             |                       |
| 02/04 | Euler's theorem                           | 13.1, 13.2       |                       |
| 02/06 | Symmetries of a regular tetrahedron       | 13.2, 13.3       |                       |
| 02/09 | Symmetries of a cube                      | 13.3, 13.4       |                       |
| 02/11 | Symmetries of a dodecahedron              | 13.4, 13.5       | **[HW 4](#hw-4) due** |
| 02/13 | **College break**                         |                  |                       |
| 02/16 | Graph theory                              | 14.1, 14.2       |                       |
| 02/18 | Graph theory                              | 14.2             |                       |
| 02/20 | Graph theory                              | 14.2, 14.3       | **[HW 5](#hw-5) due** |
| 02/23 | Introduction to algorithms                |                  |                       |
| 02/25 | Sorting algorithms                        |                  |                       |
| 02/27 | Spanning trees                            |                  |                       |
| 03/02 | Heaps, spanning trees                     |                  | **[HW 6](#hw-6) due** |
| 03/04 | **Midterm 2**                             |                  |                       |
| 03/06 | Shortest paths                            |                  |                       |
| 03/09 | Project presentations                     |                  | **Projects due**      |
| 03/11 | Project presentations                     |                  |                       |
| 03/13 | **Reading Period**                        |                  |                       |

[eg]: eucl-geom-notes.pdf

</div>

### Homeworks

#### HW 1
1. Draw a tiling of the plane that has only translational symmetry.
1. Draw a tiling of the plane that has only translational and rotational symmetry.
1. How many different symmetries does the following "bow-tie" shape (&#x22c8;) have? Write out its multiplication table.
1. Exercise 11.1.

#### HW 2
1. Exercise 11.2
1. Exercise 11.9
1. Draw a figure that has _exactly_ 5 symmetries.
1. Draw a figure that has _exactly_ 6 rotational symmetries and no reflectional symmetry. Write out the multiplication table of its symmetry group. Is there any way to match this up (as you did in Ex 11.1) with the multiplication table of $D_6$?
1. Exercise 10.1
1. Exercise 10.2
1. Exercise 10.6
1. Given any triangle, explain how to construct (with a straightedge and compass) a point whose perpendicular distances from each of the three sides are equal. How many such points are there?

#### HW 3
1. Read Appendix A carefully.
1. Let $ABC$ be a triangle with altitudes $AD$, $BE$, and $CF$. Let $H$ be the orthocenter. Find (with proof) at least one pair of similar triangles in this figure. Now try to find (with proof) three distinct triangles in this figure that are all similar.
1. Sketch a triangle, together with the perpendicular bisectors of its sides, such that the circumcenter lies outside the triangle. Make sure your sketch is convincing!
1. Sketch a triangle, together with the altitudes on each side, such that the orthocenter lies outside the triangle. Make sure your sketch is convincing!
1. (Not to be handed in) Is it possible to have triangles whose centroid or incenter lies outside the triangle? Either try to find such triangles, or try to give an argument that this is not possible. This problem is tricky, so try your best!
1. Consider a circle with center $O$. Suppose that $AC$ is any one diameter of the circle, and that $B$ is any other point on the circle. Prove that the angle $ABC$ equals $90$ degrees. (Hint: extend $BO$ so that intersects the circle again at $B'$, and try to show that $ABCB'$ is a rectangle.)
1. Let $ABC$ be a triangle with medians $AD$, $BE$, and $CF$ and centroid $G$.
    1. Prove that the triangles $ABD$ and $ADC$ have the same area.
    1. Prove that the triangles $GBD$ and $GDC$ have the same area.
    1. Prove that each of the six small triangles ($GBD$, $GDC$, $GCE$, $GEA$, $GFA$, $GFB$) have the same area. (Hint: first compare the areas of triangles $ABD$ and $CFB$ take away the common area.)
    1. Finally, this shows that the area of triangle $ABG$ is twice the area of triangle $BGD$. Conclude that the ratio of the lengths of $AG$ to $GD$ is $2:1$. (Similar results will be true for the other two medians.)

#### Midterm 1 Practice
1. Read all the sections that we covered so far from the book.
1. Read the geometry notes that I uploaded.
1. Review what a group is, especially in the context of symmetries: given a shape, you should be able to write down the multiplication table of its symmetry group.
1. List all the subgroups of $D_6$.
1. Draw a shape that is not an equilateral triangle, but which has the same symmetry group $D_6$.
1. Draw a shape that has the symmetry group $\mathbb{Z}/4\mathbb{Z}$.
1. Consider a triangle $ABC$. Let $D$, $E$, $F$ be the midpoints and $P$, $Q$, $R$ be the feet of the altitudes of $BC$, $CA$, and $AB$ respectively. Let $P'$ on $BC$ be the reflection of $P$ across $D$. Construct $Q'$ and $R'$ in an analogous way on $CA$ and $AB$. Prove that $AP'$, $BQ'$, $CR'$ are concurrent.
1. Let $r$ be the _inradius_ of a triangle $ABC$, that is the radius of the incircle. Let $s = (BC + CA + AB)/2$. Prove that the area of triangle $ABC$ equals $s\cdot r$. (Hint: Draw the lines $AI$, $BI$, and $CI$.)

#### HW 4
1. Read chapters 12 and 13.
1. Make an origami tetrahedron and bring it to class (as per the directions in the email). Please bring this on Friday, February 06.
1. Make an origami dodecahedron and bring it to class on Wednesday, February 11.
1. Exercise 12.6
1. Exercise 12.8
1. Label the vertices of a regular tetrahedron as $1$, $2$, $3$, $4$. Then make a list of all the rotational symmetries of this tetrahedron, along with a description of the axis of rotation of that transformation.
1. Using the counting method that we used in class to count the number of rotational symmetries of a tetrahedron and cube, find the number of rotational symmetries of the octahedron, dodecahedron, and icosahedron.

#### HW 5
1. Exercise 13.10
1. Exercise 13.11
1. Consider a cube with faces numbered like a die. Let $d$ be the body diagonal that joins the "1-2-3" corner to the "4-5-6" corner. Find a rotational symmetry of the cube that preserves $d$, but permutes the remaining three body diagonals cyclically. Write this symmetry down in cycle notation according to the cube faces. Explain your answer!
1. A _tree_ is any graph that is _connected_ (any two vertices can be joined by a path), and that has no cycles. Draw all possible different (non-isomorphic) trees on five vertices. Try to explain why you have found all of them.
1. Exercise 14.2
1. Exercise 14.3

#### HW 6
1. Read Chapter 14 (even parts that we didn't cover in class)
1. Exercise 14.6
1. Exercise 14.7
1. Look up the Petersen graph (on the internet or elsewhere) and draw it. Given that the shortest-length cycle in this graph has length 5, prove that this graph is non-planar.
1. Exercise 14.10 (Hint: Try to draw $K_5$ on a torus without any crossing edges.)
1. Exercise 14.16

#### Midterm 2 Practice
1. Read Chapters 12, 13, and 14.
1. Exercise 12.1
1. List all permutations of $\{1,2,3,4\}$ that take no element to its starting position. Use cycle notation.
1. Exercise 13.6. 
1. Exercise 13.12
1. Consider the following convex polyhedron: it has four faces that are regular hexagons, and four faces that are equilateral triangles. How many vertices, edges, and faces must it have?
1. What is the smallest number of colours needed to give a vertex colouring of the Petersen graph? Try this yourself before looking up the answer!
1. Exercise 14.9
1. Exercise 14.20
1. Suppose that $G$ and $H$ are two graphs such that $\overline{G}$ and $\overline{H}$ are isomorphic. Must $G$ and $H$ be isomorphic?

----

### Older announcements
{% for post in site.categories['113win15'] offset: 3%}
* **{{ post.date | date: "%a %b %d" }}:** {{ post.content | remove: '<p>' | remove: '</p>' }}
{% endfor %}
