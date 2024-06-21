select 
geography, geographycode, total_population,
age0to4 as toddlers,
age5to7 + age8to9 as kids,
age10to14 + age15 + age16to17 + age18to19 as teenagers,
age20to24 + age25to29 + age30to44 +age45to59 + age60to64 as working_professionals,
age65to74 + age75to84 + age85to89 + age90andover
from age