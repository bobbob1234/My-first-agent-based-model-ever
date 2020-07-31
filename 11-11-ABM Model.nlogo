extensions[array]


breed[racing-teams race-team]
breed[drivers driver]
breed[authorities authority]


globals
[
  z ;; cross product initalized to zero
  list1Vals
  my-tiers
  range-var
  memory-size
  disconnected-drivers




]
racing-teams-own
[
  strategy ;; virtually strategic choices looking to maximise technology_level , car quality, winnings
  car-quality ;; assume that every team has same quality
  technology-level ;; measure of work on car, access to knowledge,, closely correlated to tech_breakthrough_rate
  team-history ;; history of team over cycles(seasons) + generations(races)
  my-tier ;; based on winnings, select three my-tiers that best describes a racing team
  sponsor-appeal ;; random at first, but can be weighted on team-history,my-tier and winnings
  supplier-quality ;;
  tbr ;; speed of innovation
  starting-money ;; initalize a
  grid ;; starting_position, variable over race
  team-no ;; number of team max(10)
  gen;; iteration
  performance-effects
  lap-history
  links-contained;; netlogo variable, used to record links that are attached to specefic agent
  total-score ; sum of car-quality + tech-level + sponsor-appeal + starting-money
  starting-gen;
  gen-current;
  newly-hatched;

]

drivers-own
[
  experience
  loyalty
  driver-skill  ;; a random function, but increases over a combination of experience + wins
  driver-history
  connected
  grassroot

]



patches-own[
  track_flag
]

to setup
  ca
  produce-track
  setup-authority
  setup-racing-teams
  setup-drivers
  layout-spring racing-teams links 1 10 1
  reset-ticks
end

to go
  ;update-race
 update-racing-teams
 update-generation
 ; update-driver-pool


  tick
end
to-report cross [x y]
  report (x - z) * (y - z) - (x - x) * (y - z) ;; 3D cross-product, but defined in 2D
  	


end

to produce-track
  set z 0
  ask up-to-n-of 20 patches
  [
    ;set pcolor gray
    ;set track_flag 1
    ;set heading 0

  ]
  let active_patches  patches  with [track_flag = 1]
  let convex_list_input  sort-on [pxcor]active_patches
 let U_list []
 let L_list[]
 let sz 0
 ; foreach convex_list_input
 ;[
    ;while(sz > = 2 and cross(U_list[sz -2],U_list[sz -1]) <= 0)
    ;[
      ;U_list

    ;2]
  ;]
  ;foreach L_list
  ;[
   ; while(length(U_list) > = 2 and cross(U_list[-2],U_list[-1]) <= 0)
    ;[
;
 ;   ]
  ;]
 ; ]
end

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

to setup-racing-teams
  set range-var 1
  create-racing-teams num-of-teams [
    set pcolor white
    set color green
  setxy random-xcor random-ycor
    set my-tier one-of (range range-var tier-range)
    set starting-gen 0
    set newly-hatched false




let random-var median (list 10 ((random-normal 10.1 5.2)) 15)
 set starting-money (1 / my-tier) * ((random-var)^(tier-range - my-tier))
 let range-list (range range-var tier-range)
    foreach range-list
    [
      x ->
      let agent-subset  racing-teams with[my-tier = x]
      let counts count agent-subset with[my-tier = x]
        if(counts mod 2 = 0 and counts != 0)
        [
          let result counts / 2
          let result2 counts + 1
          let list-1 (range range-var result2)
          let list-2 (range range-var result2)
          let list-3 sentence list-1 list-2

          foreach list-3
        [
if(length list-3 > 2)
          [
    let ind1 one-of (range range-var length list-3)
          ask agent-subset[
    set team-no item ind1 list-3
          ]
    set list-3 remove-item ind1 list-3

          ]
        ]
        if(length list-3 <= 1)
        [
        let ind2 one-of list-3
        ask agent-subset [
          set team-no item ind2 list-3
        ]

        ]

      ]
      while[team-no = 0]
        [
          let edit-list (range range-var tier-range)
          set team-no one-of edit-list
        ]

       if(counts mod 2 > 0 and counts = 0)
      [
        set team-no 10

      ]
    ]








    set tbr ((random-float 1 / my-tier))
    while[sponsor-appeal > 100]
    [
      set sponsor-appeal random 100 + tbr + (1 / my-tier)
    ]
    set technology-level (1 / log starting-money 10) * ((tbr) * (1 / (log starting-money 10)))

  set supplier-quality random 10
  set car-quality 100
    set memory-size 10
    set lap-history n-values (memory-size * 2) [random 100]
    set total-score (car-quality + technology-level + sponsor-appeal + starting-money)^ tbr
    set label who
  ]





end
to setup-drivers
  create-drivers  (2 * num-of-teams)
  ask drivers
  ;;; Setting of Base Stats ;;;
  [
    set color red
    setxy random-xcor random-ycor

    set experience random 100
    set loyalty random 100
    set driver-skill random 100
    set connected 1
    set grassroot false
    create-links-to n-of 1 other racing-teams [ tie ]
  ]
end
to-report  look-for-decisions

  let decision-plus-range n-values decision-width [random 10]
  let decision-minus-range n-values decision-width [random-float -10]
  let decision-range-complete sentence decision-plus-range decision-minus-range
  report one-of decision-range-complete




end




to update-race
  [

  ]
end

to update-racing-teams
ask racing-teams
 [
    set technology-level technology-level + look-for-decisions
   set total-score (car-quality + technology-level + sponsor-appeal + starting-money)
]
end

to update-generation
  let boolean-value random 2
  if(boolean-value = 1)
  [
  ask  racing-teams with-min[total-score]
  [
      ask links [
      let my-connections both-ends
      let connection  my-connections  with [member? self drivers]
      let connection-2 my-connections with[member? self drivers]

      if( connection = connection-2)
      [
      while[connection-2 = connection]
      [

let connection-3 one-of my-connections with[member? self drivers]
          set disconnected-drivers sentence connection connection-3
      ]
      ]
      if( connection != connection-2)
    [
     set disconnected-drivers sentence connection connection-2
      ]
      ]


      ask disconnected-drivers
    [
        set connected 0
        set grassroot false
      ]
      die
      ]
    let hiring-decision random 2
   if(hiring-decision = 1)
   [
      hatch-racing-teams 1
      [

      ]
    ]
    ]





end

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
585
10
1010
436
-1
-1
12.64
1
10
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
1

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
2.0
1
1
NIL
HORIZONTAL

PLOT
290
50
480
200
Global Technology Level
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"ask racing-teams\n[\ncreate-temporary-plot-pen (word \"Agent\" (who))\nset-plot-pen-color (range 1 255)\n\n]" "ask racing-teams\n[\nset-current-plot-pen (word \"Agent\"(who))\nset-plot-pen-color black\nplot technology-level\nset-plot-pen-color green\nplot total-score\n]\n\n"
PENS

SLIDER
0
90
135
123
decision-width
decision-width
0
100
100.0
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
2.0
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
415
385
478
418
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
205
210
357
255
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
375
215
522
260
Best Technology Agents
sort [who] of racing-teams with[technology-level > mean sort [technology-level] of racing-teams]
0
1
11

BUTTON
300
375
362
408
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
