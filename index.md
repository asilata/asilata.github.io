---
layout: default
title: Home
navigation_weight: 1
---

<div class="intro">

![Asilata Bapat](assets/asilata-bapat.jpg){:#mypicture}

<div>

[Mathematical Sciences Institute](http://maths.anu.edu.au/)  
The Australian National University  
Canberra, ACT, 2601, Australia


**Email:** `asilata.bapat` at `anu` dot `edu` dot `au`  
**Office:** 4.84, [Hanna Neumann Building #145](http://www.anu.edu.au/maps#show=102872)  
**Phone:** +61 2 6125 7320

</div>

</div>

### Research interests and background

My research focuses on problems in representation theory and algebraic geometry. 
I am particularly interested in the topology, geometry, and combinatorics of spaces naturally arising from representation theory.
Recently I have been thinking about Bridgeland stability conditions on triangulated categories, and associated braid group actions.
Specific topics of interest include Bridgeland stability conditions, perverse sheaves, quiver varieties, equivariant cohomology, and Bernstein--Sato polynomials.
My papers and preprints are on my [research page](/research), and my CV is available [here](assets/bapat-cv.pdf).

### Teaching
In 2021 Semester 2, I am teaching [MATH2301](https://programsandcourses.anu.edu.au/course/MATH2301) (Games, graphs, and machines). Course notes are available [here](https://asilata.github.io/ggm/2021).

Older teaching is listed on my [teaching page](teaching/).

### Current and upcoming activities

{% capture currenttime %}{{ site.time }}{% endcapture %}
{% assign activities = site.data.activities | where_exp: "activity", "activity.date > currenttime" | sort: 'date' %}
<ul>
{% for activity in activities %}
<li>
{% unless activity.current == true %}
<strong>{% if activity.display-date %}{{ activity.display-date | markdownify | strip | remove: '<p>' | remove: '</p>' }}{% else %}{{ activity.date | date: "%b %Y" }}{% endif %}:</strong>
{% endunless %}
{{ activity.content | markdownify | strip | remove: '<p>' | remove: '</p>'}}{% if activity.location %}, {{ activity.location | remove: '<p>' | remove: '</p>'}}{% endif %}
</li>
{% endfor %}
</ul>

I am in the process of rewriting the MSI LaTeX templates for exams using [org-babel](https://orgmode.org/worg/org-contrib/babel/). It is very much a work in progress, but you can view the source code (at your own risk) in [the GitHub repository](https://github.com/asilata/msi-exam-template/).

Older activities are listed on my [activities page](activities/).

