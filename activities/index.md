---
layout: default
title: Activities
navigation_weight: 4
---

## {{ page.title }}

{% assign activities = site.data.activities | where: "category","other" | sort: 'date' | reverse %}
{% assign presentations = site.data.activities | where: "category","presentation" | sort: 'date' | reverse %}
{% capture currenttime %}{{ site.time }}{% endcapture %}

### Talks and presentations
<ul>
{% for activity in presentations %}
<li>
{% capture activitime %}{{ activity.date }}{% endcapture %}
<strong>{{ activitime | date: "%b %Y" }}{% if activitime > currenttime %} (upcoming){% endif %}:</strong>
{{ activity.content | markdownify | strip | remove: '<p>' | remove: '</p>'}}{% if activity.location %}, {{ activity.location | remove: '<p>' | remove: '</p>'}}{% endif %}
{% if activity.comment %}({{ activity.comment | remove: '<p>' | remove: '</p>' }}){% endif %}
{% if activity.slides %}[<a href="{{ activity.slides }}">slides</a>]{% endif %}
</li>
{% endfor %}
</ul>

### Organization
<ul>
{% for activity in activities %}
<li>
{% unless activity.current == true %}
<strong>{% if activity.display-date %}{{ activity.display-date | markdownify | strip | remove: '<p>' | remove: '</p>' }}{% else %}{{ activity.date | date: "%b %Y" }}{% endif %}:</strong>
{% endunless %}
{{ activity.content | markdownify | remove: '<p>' | remove: '</p>'}}
</li>
{% endfor %}
</ul>

