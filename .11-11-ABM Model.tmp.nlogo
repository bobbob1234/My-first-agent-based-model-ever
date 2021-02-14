extensions[
  array
  matrix
  table
]

undirected-link-breed [twolinks twolink]
directed-link-breed [onelinks onelink]

breed[racing-teams race-team]
breed[drivers driver]
breed[cars car]
breed[temp-patches temp-patch]
breed[track-patches track-patch]
;breed[authorities authority]


globals
[
temp-patch-agent-list
convex_table
exp_table
who_table
set_exp_0
track_counter
track_base
track_range
current_track
track_ids
track_effects
literal_one
z
patch_list
point_list
init_double
point_frame
testval1
testval2
spawn_point
starting_point
ma
future_set
]
racing-teams-own
[
  strategy ;; virtually strategic choices looking to maximise technology_level , car quality, winnings
  budget
  development-func
  morale
  upgrade-rate ; initalized as a floating point number between 0 and 1 per race
  design-strategy ; net change of performance(history), prediction

]

patches-own
[
  track_no
  track_age
]

drivers-own
[
  age
  driver-rating
  experience
  ability
  racecraft
  marketability
  morale
  driver-performance
  hatched?
  track_history
  experience_current
  future_patch
 current_patch
 previous_patch
 global_patch
 testing_patch
 lap_count
 lap_check


]


;to startup
;  setup
;end

to setup
  clear-all
;  load-tracks
  set init_double (2 * num-of-teams)
  set starting_point patch 8 1
  set marker_point patch  -  -1
  ;generate-points
  ;produce-track-convexhull
  produce-track
  setup-track-data
  setup-drivers
  start-teams
  setup-teams
  join-teams
  ;join-cars
  setup-cars
  global-hide
  init-experience-current
  setup-counters
  setup-agent-exp-table
  ;set link-limit 2
  ;produce-track
  ;create-track-data
  ;setup-authority
  ;setup-racing-teams
  ;set global-matrix matrix:from-row-list [[3 5] [0 1]]
  ;layout-spring racing-teams links 1 10 1
  reset-ticks
end


to generate-points

;  ask up-to-n-of (random 30) patches
;  [
;        set pcolor white
;  ]
  set point_list[]
  set patch_list patches with [pcolor = white]
  let patch_length (range literal_one (count patch_list))
  let pxcor_input [pxcor] of patch_list
  let pycor_input [pycor] of patch_list
  foreach (patch_length) [ x ->
  let x_item item x pxcor_input
  let y_item item x pycor_input
  let point_collate list x_item y_item
  set point_list lput point_collate point_list

  ]
;    insert-item x point_list [pycor] of  item x patch_list




end

to resize-patch-map
 ask  patches
  [
        set pcolor black
  ]

foreach(range 0 length(point_frame)) [ x ->
let frame item x point_frame
let x_frame sublist frame 0 1
let y_frame sublist frame 1 2

ask patches
[
      if(pxcor = max(x_frame) and pycor = max(y_frame))
      [
        set pcolor white
      ]
]
    ]

end

to-report lex_sort
  report sort-by [[a b] -> first a < first b] point_frame
end


to produce-track-convexhull
  generate-points
  set point_frame sentence  point_list point_list
  let length_of_points length (point_list)
  let k 0

   ifelse length_of_points < 3
  [
   stop
  ]
  [
    let sorted_random_points    lex_sort
    set point_frame sorted_random_points
  foreach(range literal_one length_of_points) [ x ->
   if(k > 2)
    [
    let one item (k - 2) point_frame
    let two item (k - 1) point_frame
    let reduce_frame sentence one two
      while[k >= 2 and  ( reduce [[a b] ->  (a - 0)  * (b - 0) - (b - 0) * (a - 0)] reduce_frame <= 0 )]
    [
      set k (k - 1)
     ]
      ]

      set k k + 1
      let points item (x) point_list
      let index (k + 1)

    set point_frame replace-item index point_frame points


    ]
  let n length_of_points - 1
  let t k  + 1

    foreach(range literal_one length_of_points) [ x ->
      if(k > 2 and x > 0)
      [
    let one item (k - 2) point_frame
    let two item (k - 1) point_frame
    let reduce_frame sentence one two

while[k >= t and  ( reduce [[a b] ->  (a - 0)  * (b - 0) - (b - 0) * (a - 0)] reduce_frame <= 0 )]
    [
      set k (k - 1)
     ]
      set k k + 1
       let points item (x - 1) point_list
      let index (k + 1)

    set point_frame replace-item index point_frame points

      ]
      ]
 let interval k + 1
set point_frame sublist point_frame 0 4
  resize-patch-map
  ]


end
to produce-track


  ask patches
  [
      if((pxcor >= -8 and pxcor <= 8) and  (pycor = 2))
    [
        set pcolor white
    ]

        if((pxcor >= -8 and pxcor <= 8) and  (pycor = -8))
    [
        set pcolor white
    ]

     if((pycor >= -8 and pycor <= 2) and  (pxcor = -8))
    [
        set pcolor white
    ]


      if((pycor >= -8 and pycor <= 2) and  (pxcor = 8))
    [
        set pcolor white
    ]



;  if((pxcor >= -7 and pxcor <= 7) and  (pycor = 2))
;    [
;        set pcolor white
;    ]
;
;      if((pxcor >= -7 and pxcor <= 7) and  (pycor = -7))
;    [
;        set pcolor white
;    ]
;        if((pycor >= -7 and pycor <= 2) and  (pxcor = -7))
;    [
;        set pcolor white
;    ]
;      if((pycor >= -7 and pycor <= 2) and  (pxcor = 7))
;    [
;        set pcolor white
;    ]

  ]
end

to setup-track-data
  set literal_one 1
  let track_id_list n-values num-of-tracks [ x -> x + 1]
  let track_effects_list n-values num-of-tracks [random 100]
  set track_ids track_id_list
  set track_effects track_effects_list
  let dict table:make
  let convex table:make
  foreach (track_id_list) [ x ->
  table:put dict (item (x - 1) track_id_list) track_id_list

  ]
  let track_effect_range (range literal_one (num-of-tracks + 1 ))
  foreach(track_effect_range) [ x ->
    table:put dict x (item (x - 1) track_effects_list)

  ]


;   foreach(track_id_list) [ x ->
;    generate-points
;  produce-track-convexhull
;   let convex_hull_points point_frame
;    table:put convex x  point_frame
;
;  ]

 table:remove dict  (range  1 num-of-tracks)

  set convex_table convex
  set exp_table dict
end

to setup-drivers

  create-drivers  init_double
  ask drivers
  ;;; Setting of Base Stats ;;;
  [
    set color black
    setxy ((max-pxcor - random-xcor + min-pycor)) ((max-pycor - random-ycor) + min-pycor)
   set age 18
   set  driver-rating  false

    set experience  set_exp_o
    set ability  random 100
    let avg_exp 0
   set racecraft  ability + avg_exp
    set track_history empty_list


;     hatch-drivers  1
;    [
;      let x_c xcor
;      let y_c ycor
;      setxy x_c  y_c
;      set hatched? true
;      set color blue
;      set label "clone"
;;      set hidden? true
;  ]


;marketability
;  morale
;  driver-performance
;    create-links-to n-of 1 other racing-teams [ tie ]
  ]

end

to setup-teams
    create-racing-teams num-of-teams
  ask racing-teams
  [

    set budget random 10000
    set morale 0 ;; funciton(team performance,upgrade rate)
    set development-func 0
    set upgrade-rate random-float 1
    set design-strategy 0

  ]
end

to join-teams
  ask drivers
  [
  create-onelinks-to n-of 1 other racing-teams [ tie ]


  ]
end
to setup-cars
  create-cars init_double
  ask cars
  [
   set color black
  ]
end
to  global-hide
ask racing-teams
  [
    set hidden? true
  ]

  ask drivers
  [
    set hidden? false
  ]

  ask onelinks
  [
    set hidden? true
  ]
end

to setup-counters
  set track_counter 0
  set track_base  num-of-tracks
  view_track_range
end
 to update-counters
  set track_counter track_counter + 1
end

to view_track_range
let track_view  n-values tracks-in-play [(random  num-of-tracks) + 1]
let track_view2 sort-by < track_view
set track_range remove-duplicates track_view2

end

to update-driver-track-history

  let track_count track_counter
  let agent_range track_range
  let agent_item item track_count agent_range
  set current_track agent_item
  ask drivers
  [
      set track_history lput  agent_item  track_history
  ]


end
to setup-agent-exp-table
  set who_table table:make
  let who_range sort [who] of drivers
  foreach(who_range) [ index ->
  table:put who_table "who_number" index
  ]
  foreach(who_range) [ index ->
  table:put who_table "experience_of_agent" [experience] of driver index
  ]

end

to-report track-in-play
  report current_track
end


;to update-agent-exp-table
;  let track_id track_range
;  let exp_update num-of-tracks
;  let who_range sort [who] of drivers
;  let index (list item track-in-play table:get who_table "who_number")
;  let table:get who_table "who_number"
;print map list item track-in-play table:get who_table "who_number"
;;foreach(who_range) [[ table:get who_table who_range] -> (exp_update +  item track_id
;
;end
;to update driver-experience-on-track
;let ticks
;; freq * performance

to init-experience-current
  ask drivers
 [
    set experience_current []
  ]

end
to gain-driver-experience
let performance_prediction random 100
let agent_range track_range
let track_count track_counter
let track_seen item track_count track_range
let effect_value table:get exp_table track_seen
let difference_list list(performance_prediction)(effect_value)
let difference_value  reduce - difference_list
ask drivers
[
set experience_current lput  difference_value experience_current

  ]



end

to-report empty_list
  report []
end
to-report copy_global_table [ orig ]
  let copy table:make

  foreach ( table:keys orig ) [
    [key] -> table:put copy key ( table:get orig key )
  ]
  report copy
end

to-report set_exp_o
 let up_exp_table  copy_global_table exp_table
let keys_found  table:keys up_exp_table
foreach(keys_found) [ x ->
    table:put up_exp_table x 0
  ]

set up_exp_table  up_exp_table


 report up_exp_table
end

;to-report update-drivers
;  ;; update driver rating + experience + racecraft
; ask drivers
;  set racecraft ability + experience
;
;
;end

to go
  ;if (ticks = num-of-tracks * num-of-tracks) [stop]
  ;if(track_counter = length track_range)
  ;[
   ; set track_counter 0
    ;view_track_range
  ;]

 race
 lap-increment


    ;update-driver-track-history
    ;gain-driver-experience
    ;develop-track-on-fly
    ;start-teams

  ;]
 ; gain_experience
  ;update-counters


  ;update-race

; update-generation
;   update-racing-teams
;  ;search-develop
;  update-driver-pool
 ; retire-driver-pool


  setup-plots
  update-plots





  tick
end

to develop-track-on-fly
  let track_num track_counter
  if(track_num > 0)
  [
   set point_frame table:get convex_table track_num
   resize-patch-map
   foreach(range 0 length(point_frame)) [ x ->
let index x
let frame item index point_frame
let x_frame  max(sublist frame 0 1)
let y_frame max (sublist frame 1 2)
  create-temp-patches 1
       [
          set xcor x_frame
          set ycor y_frame
        if(x > 1)
        [
        create-twolinks-with other temp-patches
      ]
      ]
    ]
  ]
end
to start-teams
  generate-points
  let north 0
  ask drivers
  [
    set xcor 8
    set ycor 1
    set heading north
    set current_patch patch 8 1
    set previous_patch patch 8 0
  ]
;  let one item (k - 2) point_frame
;   let two item (k - 1) point_frame
;    let reduce_frame sentence one two


end
to lap-increment
  ask drivers [
  if(ticks = 0)
  [
   set lap_count 0
   set lap_check false
  ]
  if(ticks > 15)
  [
  if(future_patch = starting_point and lap_check = false)
  [
    set lap_count lap_count + 1
    set lap_check true
  ]
    if(future_patch = starting_point and lap_check = false)
  [
    set lap_count lap_count + 1
    set lap_check true
  ]
  ]
  ]
end
to race
  ask drivers
  [


if(ticks = 0)
   [
  set global_patch previous_patch
    ]

   set testing_patch current_patch
   if(previous_patch = current_patch and global_patch != previous_patch)
   [
   set previous_patch global_patch
   ]
  face next-patch
  ;if(current_patch
;    print "Agent No"
;    print [who] of self
;    print "Original Patches"
;    ask previous_patch[print pxcor print pycor]
;    ask current_patch[print pxcor print pycor]
;    ask future_patch[print pxcor print pycor]
;    print  "Original Candidates"
;    ask testval1[print pxcor print pycor]
;    print "New Candidates"
;    ask testval2[print pxcor print pycor]
  fd random-float 1
  set current_patch patch-here
  if(current_patch != testing_patch)
  [
  set previous_patch testing_patch
   ]


  ]

end

to-report next-patch

;  let choices neighbors with [ pcolor = white ]
;  ;; choose the patch closest to the goal, this is the patch the car will move to
;  let choice min-one-of choices [ distance  myself]
;  let choice
  ;; report the chosen patch
;;; Cardinal Movement Forward
  let future current_patch
  let future_x [pxcor] of future
  let future_y [pycor] of future
  let east_patch patch (future_x + 1) (future_y)
  let west_patch patch (future_x - 1) (future_y)
  let south_patch patch (future_x) (future_y - 1)
  let north_patch patch (future_x) (future_y + 1)

;;; Cardinal Movement Backward
  let current  previous_patch
  let current_x [pxcor] of current
  let current_y [pycor] of current
  let east_prev patch (current_x + 1) (current_y)
  let west_prev patch (current_x - 1) (current_y)
  let south_prev patch (current_x) (current_y - 1)
  let north_prev patch (current_x) (current_y + 1)

  if(ticks > 1)
  [
  let future2 future_patch
  let fx [pxcor] of future2
  let fy [pycor] of future2
  let future3 patch fx fy
  set future_set (patch-set (future3))
  ]

 let prev_set (patch-set (east_prev)(west_prev)(south_prev)(north_prev) (current))
 set spawn_point ( patch-set (east_patch)(north_patch)(west_patch) (south_patch))
  let candidate_locations spawn_point with [pcolor = white]
set testval1 candidate_locations
ask candidate_locations
 [
if(member? self prev_set)
  [
      set candidate_locations candidate_locations with[self != myself]
  ]
    if(ticks > 1)[
    if(member? myself future_set)
  [
      set candidate_locations candidate_locations with[self != myself]
  ]
    ]
  ]


set testval2 candidate_locations
let choice one-of  candidate_locations
set future_patch choice


;[
;ifelse(pycor < 0 and pxcor < 0)
;[
;  let choice min-one-of  candidate_locations [xcor * ycor]
;  set future_patch choice
;
;]
;[
;  let choice one-of  candidate_locations
;  set future_patch choice
;
;]
;]
;]


report future_patch

end













;end

   to setup-authority

  if(Authority-Style = "Agressive")
    [
      ;;https://www.econstor.eu/bitstream/10419/195190/1/1662796994.pdf
      ;; penalize harder - look at overall pace, introduce penalizers for specefic teams
      ;; generate hard rules
      ;; 70/30 risk reward ratio
    ]
    if(Authority-Style = "Balanced")
    [
      ;; penalize balanced - look at overall pace, graudal periods of anti competitveness
      ;; generate balanced rules
      ;; 50/50 risk reward ratio
    ]


    if(Authority-Style = "Lenient")
    [
      ;; penalize less - look at overall pace, graudal periods of anti competitveness
      ;; generate rules - influenced by majority voting
      ;; 30/70 risk reward ratio
    ]

end

;to init-racing-teams
;
;
;  set color green
;  set color-plot item 0 n-of  1 (range 0 139)
;  setxy random-xcor random-ycor
;    set my-tier one-of (range range-var tier-range)
;    set starting-gen 0
;    set newly-hatched false
; let random-var median (list 10 ((random-normal 10.1 5.2)) 15)
; set starting-money (1 / my-tier) * ((random-var)^(tier-range - my-tier))
; set tbr ((random-float 1 / my-tier))
;   set sponsor-appeal random 100 + tbr + (1 / my-tier)
;
;    set technology-level (1 / log starting-money 10) * ((tbr) * (1 / (log starting-money 10)))
;
;  set supplier-quality random 10
;  set car-quality 100
;    set memory-size 10
;    set lap-history n-values (memory-size * 2) [random 100]
;    set total-score (car-quality + technology-level + sponsor-appeal + starting-money)^ tbr
;    set label who
;    set team-no who
;
;  set decision-array fill-matrix decision-width 1 [ -> random-float 0.01]
;
;
;
;
;
;end
;to setup-global-matrix
;
;  let temp-matrix fill-matrix (decision-width * num-of-teams)  1 [ -> random-float 0.01]
;  set global-matrix temp-matrix
;
;end



;to-report compute-decision-space-racing-teams
;   let temp-list matrix:to-row-list global-matrix ;; converts the matrix to a list
;  let random-index-range (range range-var (round((decision-width * num-of-teams) * search-space-sharing-proportion)))
;  foreach random-index-range
;  [
;    x ->
;    set temp-list remove-item x temp-list ;; the new row is removed to the list
;  ]
;  let space-share (round((decision-width * num-of-teams) * search-space-sharing-proportion))
;  let space-share2 space-share * 2
;  let global-last-rows (range space-share space-share2)
;  foreach global-last-rows
;  [
;    let rand-number  random-float 0.01
;       set temp-list lput  rand-number temp-list
;    ]
;    report  temp-list
;
;
;
;
;end

;to setup-racing-teams
;  set range-var 1
;  create-racing-teams num-of-teams [ init-racing-teams]
;
;
;
;
;
;
;
;end
;
;to  setup-hatched-racing-teams
;  ask racing-teams with [newly-hatched = true]
;  [
;    init-racing-teams
;
;    set starting-gen starting-gen + 1
;    set newly-hatched true
;  ]
;
;
;end


;  ask racing-teams with [count my-links > link-limit]
;  [
;
;      ask n-of (count my-links - link-limit) my-links [die]
;
;  ]
;
;  ask racing-teams with [count my-links < link-limit]
;  [
;   let x link-limit - count my-links
;    create-links-to n-of  (link-limit - count my-links)  drivers with[count my-links = 0 ] [tie]
;    ]









;to hire-new-drivers
;
;  ask racing-teams with [count my-links < link-limit]
;  [
;   let x link-limit - count my-links
;    create-links-to n-of  (link-limit - count my-links)  drivers with[count my-links = 0 ] [tie]
;    ]
;end


;to-report  look-for-decisions

 ; let decision-plus-range n-values decision-width [random 10]
  ;let decision-minus-range n-values decision-width [random-float -10]
  ;let decision-range-complete sentence decision-plus-range decision-minus-range

  ;report one-of decision-range-complete




;end

;to  look-for-decisions
;
;if(selection-replacement = "Random")
;[
;
;]
;
;if(selection-replacement = "Absolute")
;[
;
;]
;
;if(selection-replacement = "Roulette")
;[
;
;]
;
;if(selection-replacement = "Rank")
;[
;
;]
;
;if(selection-replacement = "Informed")
;[
;
;]
;
;end




;to-report fill-matrix [n m generator]
;  report matrix:from-row-list n-values n [n-values m [runresult generator]]
;end
;
;
;
;to update-race
;  [
;
;  ]
;end
;
;to update-racing-teams
;  ask racing-teams
; [
;    set technology-level 10 ;; look-for-decisions
;   set total-score (car-quality + technology-level + sponsor-appeal + starting-money)
;
;]
;   ask racing-teams with [count my-links = 0]
;  [
;    hire-new-drivers
;
;  ]
;
;
;end
;
;to update-generation
;let boolean-decision random 2
;  if(boolean-decision = 1)
;  [
;  ask  racing-teams with-min[total-score]
;  [
;
;      die
;   ]
;
;
;
;
;
;    create-racing-teams 1
;    ask racing-teams with [ technology-level = 0]
;    [
;       set newly-hatched true
;       setup-hatched-racing-teams
;     ]
;  ]
;
;end

;  to update-driver-pool
;
;
;  create-drivers  (2 * num-of-teams)
;  ask drivers
;  ;;; Setting of Base Stats ;;;
;  [
;    set color red
;    setxy random-xcor random-ycor
;
;    set experience random 100
;
;  ]
;
;
;
;
;
;end
;













to  sim-race
  ask racing-teams
  [

  ]




end

  to-report racing-team-strategy
  let x 1
  report x

end
@#$#@#$#@
GRAPHICS-WINDOW
565
12
1018
466
-1
-1
13.5
1
9
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

CHOOSER
0
340
155
385
Authority-Style
Authority-Style
"Agressive" "Balanced" "Lenient"
0

PLOT
1668
340
1868
490
Competiveness Chart
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

PLOT
1633
133
1833
283
Competiveness Over Generations
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

PLOT
1470
165
1670
315
Policy Change Vs Competiveness
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

PLOT
1699
313
1899
463
Technology Level vs Competiveness
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean[technology-level] of racing-teams"

PLOT
1301
7
1501
157
Amount Of Money Vs Technology Level(Generations)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [starting-money] of racing-teams"
"pen-1" 1.0 0 -7500403 true "" "plot sum [technology-level] of racing-teams"

SLIDER
0
30
135
63
num-of-teams
num-of-teams
0
20
16.0
1
1
NIL
HORIZONTAL

PLOT
290
50
560
275
Global Technology Level
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"ask racing-teams\n[\ncreate-temporary-plot-pen (word \"Agent\" (who + 1))\nset-plot-pen-color color-plot\n]" "\nask  racing-teams\n[\nset-current-plot-pen (word \"Agent\"(who + 1))\n\nplot technology-level\n\n]\n\n"
PENS

SLIDER
244
495
379
528
decision-width
decision-width
0
100
33.0
1
1
NIL
HORIZONTAL

SLIDER
0
60
135
93
tier-range
tier-range
2
6
4.0
1
1
NIL
HORIZONTAL

SLIDER
0
380
155
413
grassroot-hiring-rate
grassroot-hiring-rate
0
1
0.28
0.01
1
NIL
HORIZONTAL

SLIDER
0
410
160
443
hiring-rate
hiring-rate
0
1
0.26
0.01
1
NIL
HORIZONTAL

BUTTON
139
82
267
171
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
210
290
362
335
Best Agent
[who] of racing-teams with-max[total-score]
1
1
11

TEXTBOX
295
20
565
66
Global Measurement
20
0.0
1

TEXTBOX
295
35
480
53
----------------------------------------------
11
0.0
1

TEXTBOX
25
5
175
31
Parameters\n---------
11
0.0
1

MONITOR
370
300
517
345
Best Technology Agents
sort [who] of racing-teams with[technology-level > mean sort [technology-level] of racing-teams]
0
1
11

BUTTON
138
13
267
87
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
250
444
438
496
Decision Strategy Parameters\n------------------------
13
0.0
1

SLIDER
407
494
635
527
search-space-sharing-proportion
search-space-sharing-proportion
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
275
544
459
577
decision-generation-limit
decision-generation-limit
0
100000
0.0
1000
1
NIL
HORIZONTAL

SLIDER
513
550
686
583
mutation-chance
mutation-chance
0
100
92.0
1
1
NIL
HORIZONTAL

CHOOSER
312
588
484
633
selection-replacement
selection-replacement
"Random" "Absolute" "Roulette" "Rank" "Informed"
0

SLIDER
0
95
130
128
num-of-tracks
num-of-tracks
0
1000
17.0
1
1
NIL
HORIZONTAL

SLIDER
0
130
135
163
tracks-in-play
tracks-in-play
1
num-of-tracks
9.0
1
1
NIL
HORIZONTAL

MONITOR
1020
10
1092
55
Lap_Count
10
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

1) https://pdf.sciencedirectassets.com/280203/1-s2.0-S1877050918X00064/1-s2.0-S1877050918303776/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEHAaCXVzLWVhc3QtMSJGMEQCIEDqHQJ6Jy1im65OKEAOe%2FoBpnQr4EtgvBh2B6mbRTZxAiAVFsug7jqQUfQQwwtoXxjqt1b%2FEFrwr0j6AYYE2yTTsiq0AwgoEAMaDDA1OTAwMzU0Njg2NSIMAIk0JxcZibwy4phsKpEDkVPLFqq54ZnWqYFuesskxkU8djrr0vBi4Vp%2BLz0ItSF0cZpmMMgFBSFxEFvXt8dypdNfYBEoDithayhg1c5sUKVuxHtpxTXEEld6nXchDN986rxco%2FQ5cxvq7l5Sfx1P4FCR%2FxGAs%2BfWz40ZA4mVqoWHBc92U5KG50jYe5ZabsZOWzGez2qHXdqCSVODDpSOcEwzx6YGSHhh5cMAnefzoVHkXOhIdq%2BIGTgjd4%2FRz39zvsPCkHqjetY4b7aHBmyXej9jc2WfuKVsgJXrmvu3gTnARvYJcjcRloJTGvgwk9uNPh%2BI9959VrRkgDtoIISLr8aMl65GZQsxigVSpT8zhfK1d2DtUZnDKyl7%2FmdvxzFp5Gkz4jzSa3guBTwNESzKQIyFr7YGa8Cua3bGEv1cMXmCtHRhyR72HHc38ezaNjHaxMGyHiocXwNEvuU%2FOJRQJ3MW2YEVHYCKW3%2FYM87CvAJSvd239gNZRP6S36z%2BSfhjX7XgEdrG509YDKDnuZCL99%2B9xA%2BuZvPn9HK%2BrfkAKo0wiIvV%2BAU67AEF2h1I8BOU61yWgfNQbiPc7FurhsbQ%2BboNadAnYe%2F6ZiF5iwuHHX8rNCvttRaARjUqOfCQnhU19XLu69LHkZg28JJyQwhxk6Xorygh6gp0WiVjDJi%2BvQ7hUUt0GrpvZSm%2BPsqMZm28BhvhF77xkAA6NulVkpNEcbCv8mFCEze0syYFbiv3CX89sDyJfwmbCchAf1GvVhXGnUHSqXq0zUzv4PA6EcmyKRLtYdUgfPVes7C4DpnpyqcFHGPr2Rrn9hZcoXUipy4Vmf0okVSHdkdgXRRypLxAOPSV8w0xLRVn2bcC4h6oCOTKMtnMrw%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20200720T074112Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYSPGBB5FB%2F20200720%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=5294ed81881cb4264e6425b4014c7e046d2566cbba370c06383a8c7fc6b23f9a&hash=b62b03b8d8a2a6a32b03d9b5b231981f19ffcdaa4eed1a46be7084d9be1c8b01&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S1877050918303776&tid=spdf-f068e89f-aeb0-43f9-b70e-addcd5d59de8&sid=9609a67686e6894f4369b8d32746d7cbedbagxrqb&type=client

2) https://beta.vu.nl/nl/Images/werkstuk-sulsters_tcm235-877826.pdf


## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="grassroot-hiring-rate">
      <value value="0.28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-of-teams">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tier-range">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Authority-Style">
      <value value="&quot;Balanced&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-generation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="search-space-sharing-proportion">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-chance">
      <value value="112.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hiring-rate">
      <value value="0.26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-width">
      <value value="69"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-and-replacement">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
