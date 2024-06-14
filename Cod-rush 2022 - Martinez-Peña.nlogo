extensions [nw table]      ; network extension;  table extension to represent curves of cod population size at each tick; do I need an array extension?

undirected-link-breed [ undirected-edges undirected-edge ] ; directionality of the fow of information is given by the position of the seed
directed-link-breed [ directed-edges directed-edge ]


globals [
  ;;;;;;;;;;;;;;;; MECHANISM RELATED:
  cod_density
  fishers_just_switched
  proportion_of_fishers_switched
  peer_effects ; This variable contains the proportional value of peer effects on propensity to switch
  perception_of_opportunity_effects ; This variable contains the proportional value of perception of opportunity effects on propensity to switch
  base_propensity; this is the proportional effect of the initially set base propensity for the fishers that switched, which changes each tick
  global_threshold ;
  t0_cod_population ; new-line
  cod_population ; new-line
  regenAmount ; new-line
  extractAmount; new-line
  carrying_capacity ;
  other_fishers;
  total_op_fish_switched;
  looping_list
  all_fishers
  proportion_of_switched_back_fishers
  switched_fishers_per_tick
  back_switchers_per_tick
  active_opp_fishers
  msy ; maximum sustainable yield
  msy_p ; maximum sustainable yield proportional
  msfe_p ; maximum sustainable fishing effort to be plotted at the scale of opportunistic fishers
  msfe ; maximum sustainable fishing effort
  mcd ;
  mf ;

  ;;;;;;;;;;;;;;;;; NETWORK RELATED:
  highlighted-node                ; used for the "highlight mode" buttons to keep track of the currently highlighted node
  highlight-bicomponents-on       ; indicates that highlight-bicomponents mode is active
  stop-highlight-bicomponents     ; indicates that highlight-bicomponents mode needs to stop
  highlight-maximal-cliques-on    ; indicates highlight-maximal-cliques mode is active
  stop-highlight-maximal-cliques  ; indicates highlight-maximal-cliques mode needs to stop
  average-centrality              ;
  number-of-edges                 ;
  number-of-non-isolates          ;
  global-clustering               ;
  average-shortest-path           ;
  seed-centrality                 ;

]


turtles-own
[
  threshold_SI ; ---------------------------------- This is the variable that determines whether fishers switch from not fishing to fishing. When the value reaches 100, they switch.
  degree_centrality ;--------------------------------------- Number of links for each turtle
  peer_influence   ; -------------------------------------- The influence from neighbours that just switched
  times_exited     ; --------------------------------------
  activity         ; -------------------------------------- This variable takes values 1 and 2. 1 means that fishers are actively fishing and 2 means that they are doing something else. The value changes when they switch
  IPUE             ; -------------------------------------- Acronym of income per unit of effort. This variable is equsl to fish density * fishing capacity. It includes time spent fishing and assumes equal effort for all fishers.
  perception_of_potential_income  ; ----------------------- This variable is only relevant for opportunistic fishers. The value is defined by the value of IPUE that is communicated by fishers when
  threshold_EI
  switched_neighborhood     ; the number of links to ooportunistic fishers that switched to fishing during the last tick ( i.e.just_switched = 1)
  livelihood_strategy ; Is the fisher opportunistic or a regular fisher
  just_switched    ; -----------------------------
  local-clustering ;
  proportion_in-adopters
  proportion_out-adopters;
  current_occupation ;
  probability_SI_in
  probability_SI_out
  probability_EI_in
  probability_EI_out
  mensaje
  roll-called
  final_TIR
  final_POPI
  final_tick
  last_mssg_sender
  combined_threshold
  threshold_EI-out
  EI_t.update
  EI-in_crossed
  fishing_capacity
]


links-own [
  strength
  info_on_pop ; information about perception of opportunity
]

;patches-own [density]

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> TO DO
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; create the scenarios
; create plots of op fishers switched
; Give fishers different fishing capacity -> Make sure that a when opp fishers receive info from two sources that spread different values, these are weighted e.g. average value, or max or min



to-report get-links-to-use
  report undirected-edges
end

; >>>>>>>>>>>>>>>>>>>SETUP

to setup-network
  ca
  set-current-plot "Degree distribution"



end


to calculate_global_clustering
  let closed-triplets sum [ nw:clustering-coefficient * count my-links * (count my-links - 1) ] of turtles
  let triplets sum [ count my-links * (count my-links - 1) ] of turtles
  let final_calculation (closed-triplets / triplets)
  set global-clustering final_calculation
end


to setup
  ca

  small-world-ring
  ask turtles
  [
      set shape "person"
      set size 1.5
      set color red
      set activity 2
      set current_occupation "not-fishing"
      set livelihood_strategy "opportunistic_fisher"
      set threshold_SI random-normal SI_threshold Sd_SI
      if threshold_SI < 0.05
      [
        set threshold_SI 0.05
      ]
      if threshold_SI > 1
      [
        set threshold_SI 1
      ]
      set threshold_EI random-normal EI_threshold Sd_EI
      if threshold_EI > 1
      [
        set threshold_SI 1
      ]
      set combined_threshold (threshold_SI + threshold_EI)
      set peer_influence 0
      set times_exited 0
      set IPUE 0
      set perception_of_potential_income 0
      set degree_centrality (count my-links)
      set local-clustering (nw:clustering-coefficient)
      set mcd 0
      set mf 0
      set label who
      set looping_list []
      set mensaje 0
      set roll-called "pending"
      set fishing_capacity 1
  ]

  If ( Cod.scenario = "Logistic_growth" ) ; new-line
  [
    set other_fishers background.effort
    ;set other_fishers (FishersPerOpFisher * nb-nodes)
    ;print (word "full time fishers: " other_fishers "+ oportunistic fishers: " nb-nodes " = total: " (nb-nodes + other_fishers))
    ;set t0_cod_population ((nb-nodes + other_fishers) * fish-fisher_ratio); new-line  ; es mejor que pongas un slider que controle t0_cod_population en lugar de fish-fisher ratio?
    set t0_cod_population t0.fish.population
    ;print ( word "initial_population = " t0_cod_population); new-line
    set cod_population t0_cod_population
    set cod_density (cod_population / t0_cod_population)
  ]

   If ( Cod.scenario = "linear_growth")
   [
    set other_fishers background.effort
    ;set other_fishers (FishersPerOpFisher * nb-nodes)
    ;print (word "full time fishers: " other_fishers "+ oportunistic fishers: " nb-nodes " = total: " (nb-nodes + other_fishers))
    ;set t0_cod_population ((nb-nodes + other_fishers) * fish-fisher_ratio); new-line  ; es mejor que pongas un slider que controle t0_cod_population en lugar de fish-fisher ratio?
    set t0_cod_population 100
    ;print ( word "initial_population = " t0_cod_population); new-line
    set cod_population t0_cod_population
    set cod_density (cod_population / t0_cod_population)
  ]


  set average-centrality ((sum [degree_centrality] of turtles) / ( count turtles))
  set number-of-edges    (count links)
  set number-of-non-isolates  ((count turtles) - (count turtles with [degree_centrality = 0 ]))
  set average-shortest-path  (nw:mean-path-length)

  ;average-centrality   number-of-edges  number-of-non-isolates    global-clustering    average-shortest-path

  calculate_global_clustering   ; REMOVE SEMICOLON
  seed ; REMOVE SEMICOLON

  set all_fishers (other_fishers + ( count turtles with [activity = 1]))


  reset-ticks
end

to seed
  ask n-of full.time.fishers turtles
    [
      ifelse ((count my-links ) = 0)
      [
        repeat 1 [seed]
        print "repeat seeding"
      ]
      [
        set color 85
        set activity 1
        set livelihood_strategy "fisher"
        set current_occupation "full-time-fishing"
        ;set times_info_received (ticks + 1 )   ; The code should work even if this number is 1 - TRY!!!
        set roll-called "none"
        set mensaje "none"
      ]
      set seed-centrality ([degree_centrality] of n-of 1 turtles with [color = 85] )
  ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to layout-turtles

  if layout = "radial" and count turtles > 1 [
    let root-agent max-one-of turtles [ count my-links ]   ;;;WHO´s root-agent?
    layout-radial turtles links root-agent
  ]
  if layout = "spring" [
    let factor sqrt count turtles
    if factor = 0 [ set factor 1 ]
    layout-spring turtles links (1 / factor) (14 / factor) (1.5 / factor)
  ]
  if layout = "circle" [
    layout-circle sort turtles max-pxcor * 0.9
  ]
  if layout = "tutte" [
    layout-circle sort turtles max-pxcor * 0.9
    layout-tutte max-n-of (count turtles * 0.5) turtles [ count my-links ] links 12
  ]
  display
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Centrality Measures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to betweenness
  centrality [ -> nw:betweenness-centrality ]
end

to eigenvector
  centrality [ -> nw:eigenvector-centrality ]
end

to closeness
  centrality [ -> nw:closeness-centrality ]
end

; Takes a centrality measure as a reporter task, runs it for all nodes
; and set labels, sizes and colors of turtles to illustrate result
to centrality [ measure ]
  nw:set-context turtles get-links-to-use
  ask turtles [
    let res (runresult measure) ; run the task for the turtle
    ifelse is-number? res [
      set label precision res 2
      set size res ; this will be normalized later
    ]
    [ ; if the result is not a number, it is because eigenvector returned false (in the case of disconnected graphs
      set label res
      set size 1
    ]
  ]
  normalize-sizes-and-colors
end

; We want the size of the turtles to reflect their centrality, but different measures
; give different ranges of size, so we normalize the sizes according to the formula
; below. We then use the normalized sizes to pick an appropriate color.
to normalize-sizes-and-colors
  if count turtles > 0 [
    let sizes sort [ size ] of turtles ; initial sizes in increasing order
    let delta last sizes - first sizes ; difference between biggest and smallest
    ifelse delta = 0 [ ; if they are all the same size
      ask turtles [ set size 1 ]
    ]
    [ ; remap the size to a range between 0.5 and 2.5
      ask turtles [ set size ((size - first sizes) / delta) * 2 + 0.5 ]
    ]
    ask turtles [ set color scale-color red size 0 5 ] ; using a higher range max not to get too white...
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to generate [ generator-task ]
  setup-network
  ; we have a general "generate" procedure that basically just takes a task
  ; parameter and runs it, but takes care of calling layout and
  run generator-task
  layout-turtles
  update-plots
end


to small-world-ring
  generate [ -> nw:generate-watts-strogatz turtles get-links-to-use fishers neighborhood.size rewire.prob ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading network files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to save-matrix
  nw:set-context turtles get-links-to-use
  nw:save-matrix "C:/Users/rodma51/Dropbox/PhD/Paper IV/Netlogo/matrices/matrixAttempt.txt"
  ;nw:save-matrix "matrix.txt"
end

to load-matrix
  ;if ne
  generate [ -> nw:load-matrix "network_6_.txt" turtles get-links-to-use ]
end

to save-graphml
  nw:set-context turtles get-links-to-use
  nw:save-graphml "demo.graphml"
end

to load-graphml
  nw:set-context turtles get-links-to-use
  nw:load-graphml "demo.graphml"
end

;;;;;;;;;;;;;GO
;;;;;;;;;;;;;;;;;;;;;;GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GO
to Go
  if (ticks = tick_span ) [ stop ]
  If ( Cod.scenario = "Observed_scenario" )
  [ create-CodScenario3-table ]
  update-fish-pop
  switch-back
  follow-or-not-to-follow; opportunistic fishers influenced by neighbors that switched in the last tick update their propensity to switch. If it reaches 100, they switch
  get-direct-info
  if (count turtles with [livelihood_strategy = "opportunistic_fisher"] ) >= 1
  [
  track-changes
  ]
  fish; In this part code fishers go fishing, meaning that they change their IPUE. IPUE can serve as a monitor variable for this process

  tick
end




to switch-back


   ask turtles with [just_switched = 2]
  [
    set just_switched "gone_and_back"
  ]

  ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CALCULATING PROBABILITY TO EXIT FISHING
  ask turtles with [ current_occupation = "fishing" ]
  [

    ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CALCULATING PROBABILITY TO EXIT FISHING DUE TO SOCIAL INFLUENCE
    let neighbours (count link-neighbors with [livelihood_strategy = "opportunistic_fisher"] )
    ;print (word"""neighbours ="" "neighbours)
    let exited_neighbour (count link-neighbors with [current_occupation = "exited"])  ;Only agents that have switched back produce this social influence, because only when they change it is a signal of good opportunity - -It's no signal that full time fishers keep fishing
    ;print (word"""exited_neighbour ="" "exited_neighbour)
    ifelse SI_in = true
    [
      set proportion_out-adopters ( exited_neighbour  / neighbours); ATTENTION - when there are no neighbours, this lines gives error because it divides neighbour_fishers/ zero (neighbours)
      let exponent_SI_out ( (threshold_SI - proportion_out-adopters) * (slope_SI) )
      let denominator_SI_out (exp exponent_SI_out)
      set probability_SI_out ((1 / (1 + denominator_SI_out))/ 2) ;
      ;print (word """ID = "" "self """ tick = "" " ticks """ popi_fx_th = "" "popi_fx_th  """ threshold_EI-out = " threshold_EI_out );print (word"""tick="" "ticks  """  ID="" "self  """  threshold="" " threshold """  popi_fx_th ="" "popi_fx_th """  proportion_in-adopters ="" "proportion_in-adopters  """  exponent ="" "exponent """  denominator(total) ="" "denominator """   probability ="" "probability)
    ]
    [
      set probability_SI_in 0
    ]


    ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CALCULATING PROBABILITY TO EXIT FISHING DUE TO EXPECTED INCOME
    ifelse EI_out = true
    [
      let popi_fx_th ( (perception_of_potential_income - 1)); * fish-threshold_relation)
      ;if  popi_fx_th < 0   ; Este condicional es para mantener los valores del threshold por debajo de 1 - si no se usa, en las líenas más abajo se traduce en -.01 en popi_fx_th. Sin embargo, tal vez tiene sentido que cuando la abundancia de pez no cambia, la probabilidad de que pescadores oportunistas se sumen reciba in cierto penalti
      ;[
      ;  set popi_fx_th 0
      ;]
      ;print (word"""popi_fx_th ="" "popi_fx_th)
      let threshold_EI_out ((threshold_EI-out - thr-exit ) * ( - 1))
      ;print (word"""ID="" "self """threshold_EI-out: "" "threshold_EI-out """- thr-exit="" "thr-exit  """// threshold_EI_out= "" "threshold_EI_out )
      ;print (word """ID = "" "self """ tick = "" " ticks """ popi_fx_th = "" "popi_fx_th  """ threshold_EI-out = " threshold_EI_out )
      let exponent_EI_out ( (threshold_EI_out + popi_fx_th) * (slope_EI) )
      let denominator_EI_out (exp exponent_EI_out)
      set probability_EI_out ((1 / (1 + denominator_EI_out))/ 2) ;
      ;print (word  """ID="" "self  """probability_EI_out = "" "probability_EI_out)
    ]
    [
      set probability_EI_out 0
    ]
    ;if prob_vs_det = "probabilistic"
    let random_uniform_out random-float 1
    ;print (word"""ID="" "self """probability_SI_in="" probability_SI_in """+ probability_EI_in="" probability_EI_in""" random_uniform#"""=random_uniform_in   )
    if (probability_SI_out + probability_EI_out )> random_uniform_out  ; (probability_SI_out + probability_EI_out - (0.5 - probability_SI_out) )> random_uniform_out
    [
      set activity 2
      set color green
      set just_switched 2
      set current_occupation "exited"
      ;set size size + 1
      set times_exited times_exited + 1
    ]
  ]
end

to follow-or-not-to-follow



  ask turtles with [ just_switched = 1 ] ; This command should not be moved.
  [
    set just_switched "already switched"
  ]

  ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CALCULATING PROBABILITY TO ENTER FISHING
  ask turtles with [ activity = 2 ]
  ;ask turtles with [ current_occupation = "not-fishing" ]
  [
     ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CALCULATING PROBABILITY TO ENTER FISHING DUE TO SOCIAL INFLUENCE
    let neighbours (count link-neighbors with [livelihood_strategy = "opportunistic_fisher"] )
    ;print (word """  ID= "" "self """ neighbours = "" "neighbours)
    let neighbour_fishers (count link-neighbors with [current_occupation = "fishing"])  ;Only agents that have switched produce this social influence, because only when they change it is a signal of good opportunity - -It's no signal that full time fishers keep fishing
    ;print (word """  ID= "" "self """ neighbour_fishers = "" "neighbour_fishers)
    ifelse SI_in = true
    [
      set proportion_in-adopters ( neighbour_fishers  / neighbours); ATTENTION - when there are no neighbours, this lines gives error because it divides neighbour_fishers/ zero (neighbours)
      let exponent_SI_in ( (threshold_SI - proportion_in-adopters) * (slope_SI) )
      let denominator_SI_in (exp exponent_SI_in)
      set probability_SI_in ((1 / (1 + denominator_SI_in))/ 2) ;
      ;print (word """  ID="" "self  """  threshold="" " threshold """    proportion_in-adopters ="" "proportion_in-adopters  """  exponent ="" "exponent_SI_in """  denominator(total) ="" "denominator_SI_in """   probability_SI_in ="" "probability_SI_in)
    ]
    [
      set probability_SI_in 0
    ]


    ;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  CALCULATING PROBABILITY TO ENTER FISHING DUE TO EXPECTED INCOME
    ifelse EI_in = true
    [
     let popi_fx_th ( (perception_of_potential_income - 1)); * fish-threshold_relation)
     ;if  popi_fx_th < 0   ; Este condicional es para mantener los valores del threshold por debajo de 1 - si no se usa, en las líenas más abajo se traduce en -.01 en popi_fx_th. Sin embargo, tal vez tiene sentido que cuando la abundancia de pez no cambia, la probabilidad de que pescadores oportunistas se sumen reciba in cierto penalti
     ;[
     ;  set popi_fx_th 0
     ;]
     ;print (word"""popi_fx_th ="" "popi_fx_th)
      let exponent_EI_in ( (threshold_EI - popi_fx_th) * (slope_EI) )
      let denominator_EI_in (exp exponent_EI_in)
      set probability_EI_in ((1 / (1 + denominator_EI_in))/ 2) ;
    ]
    [
      set probability_EI_in 0
    ]

    let random_uniform_in random-float 1
    ;print (word"""ID="" "self """probability_SI_in="" "probability_SI_in """+ probability_EI_in="" "probability_EI_in """ > random_uniform# = "" " random_uniform_in   )
    if (probability_SI_in + probability_EI_in) > random_uniform_in
    [
      set color yellow
      set activity 1
      set just_switched 1
      set current_occupation "fishing"
      set final_POPI perception_of_potential_income
      set final_tick ticks
      if (cod_density - 1) > threshold_EI
      [
        set threshold_EI-out (cod_density - 1)
      ]
      set EI_t.update (EI_t.update + 1)
      set EI-in_crossed "passed"

      ;set times_info_received (ticks + 1)
    ]
  ]



end


to order_flow
  ask turtles with [mensaje = "none"]
  [
    ask link-neighbors with [mensaje = 0] ; Solo funciona al principio de la simulación
    [
      set mensaje 1
      set roll-called "yes"
      ;set color green
      ;set shape "flag"
    ]
  ]

  if (ticks >= 1)
  [
    ;print "ordering flow active"
    ask turtles with [mensaje = 1]
    [
    ask link-neighbors with [mensaje = 0]
      [
      set roll-called "yes" ;
      set mensaje 1   ; mensaje es para senialar hasta donde llega la lista
      ]
    ]
  ]

end

to fill_list
  ifelse ( (count turtles with [mensaje = 1]) >= (count turtles with [livelihood_strategy = "opportunistic_fisher"])) ; En caso de que haya una red partida o en caso de que no todos los pescadores cambien,
  [];print("La lista ya no puede crecer mas")]
  [
   set looping_list fput ([who] of turtles with [roll-called = "yes"]) looping_list
   ;print looping_list
  ]

end




to get-direct-info
  ask turtles with [activity = 2]
  [
    set perception_of_potential_income cod_density
    ;set size size + 1
  ]

end





to track-changes

  set all_fishers (other_fishers + ( count turtles with [activity = 1]))
  set total_op_fish_switched total_op_fish_switched + count turtles with [just_switched = 1]
  set fishers_just_switched fishers_just_switched + count turtles with [just_switched = 1] - count turtles with [just_switched = 2];set fishers_just_switched count turtles with [just_switched = 1] ;
  ;set fishers_just_switched fishers_just_switched - count turtles with [just_switched = 2]
  set proportion_of_fishers_switched ((fishers_just_switched / count turtles with [ livelihood_strategy = "opportunistic_fisher"]) * 100)
  set proportion_of_switched_back_fishers ((count turtles with [current_occupation = "exited"] / count turtles with [ livelihood_strategy = "opportunistic_fisher"]) * 100)
  set switched_fishers_per_tick (count turtles with [just_switched = 1])
  set back_switchers_per_tick (count turtles with [just_switched = 2])
  set active_opp_fishers (count turtles with [current_occupation = "fishing"])

  if cod_population > mf
   [
    set mf cod_population
   ]
end


to fish
  ask turtles with [ activity = 1 ]
  [
    set IPUE (cod_density); This is the income per unit of effort of active fishers
  ]
   ;print (word "cod density  = " cod_density)

  ask turtles with [ current_occupation = "fishing" ]
  [
    set perception_of_potential_income IPUE
  ]

   If ( Cod.scenario = "Logistic_growth" ) ; new-line
  [
    if fish.mortality = true
    [
    set extractAmount ((sum [fishing_capacity] of turtles with [ activity = 1 ]) + (other_fishers))  ;new-line
    ;print (word "Otros  = " extractAmount)
    ;print (word "Amount extracted = " extractAmount)
    set cod_population (cod_population  - extractAmount)  ;new-line
      if cod_population <= 0
      [
        set cod_population 0
      ]
    ]
  ]

  If ( Cod.scenario = "linear_growth")
  [
    if fish.mortality = true
    [
    set extractAmount ((sum [fishing_capacity] of turtles with [ activity = 1 ]) + (other_fishers))  ;new-line
    ;print (word "Otros  = " extractAmount)
    ;print (word "Amount extracted = " extractAmount)
    set cod_population (cod_population  - extractAmount)  ;new-line
    ]
  ]

end


to update-fish-pop ;new-line , new procedure actually
   If ( Cod.scenario = "Logistic_growth" ) ; new-line ; here is where we bring the regime shift in
  [
   ; ifelse (cod_density <= regime_shift_threshold)
   ; [
    ;  set carrying_capacity ( regime_shift_threshold * t0_cod_population )
      ;print (word "carrying_capacity = " carrying_capacity)
    ;]
    ;[
      set carrying_capacity carrying.capacity ; YOU SHOULD REDUCE THIS
      ;print (word "carrying_capacity = " carrying_capacity) ;;; YOU SHOULD REDUCE THIS
    ;]

    set regenAmount (cod_population * log.rate * (1 - (cod_population / carrying_capacity))) ; regenAmount round(fish-stock * growth-rate * (1 - (fish-stock / PATCH-CARRYING-CAPACITY)))
    ;print (word "regenAmount = " regenAmount)
    ;print (word "cod_population = " cod_population)
    ;print (word "growth-rate = " growth-rate)
    ;print (word "carrying_capacity = " carrying_capacity )
    set cod_population (cod_population + regenAmount)
    set cod_density (cod_population / t0_cod_population)
    ;print (word "cod_population = " cod_population  "regeneration = " regenAmount)
    set msy (carrying_capacity / 2 )
    set msfe (( carrying_capacity * log.rate) / 4)
    set msfe_p ( msfe - background.effort)
    set msy_p (msy / t0_cod_population)
    ;print (word "msy: " msy "   msy_prime: " msy_prime)

  ]

    If ( Cod.scenario = "linear_growth")
  [
    set regenAmount linear.rate
    set cod_population (cod_population + regenAmount)
    set cod_density (cod_population / t0_cod_population)
  ]



     If (cod_population < 0)
    [
      set cod_population 0
    ]

 if mcd < cod_density
 [
   set  mcd cod_density
  ]








end

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COLLAPSE SCENARIOS
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COLLAPSE SCENARIOS
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COLLAPSE SCENARIOS

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>TO DO : make that the no change scenario can take different values
to create-CodScenario1-table ; This is the scenario where cod population remains always unchanged
 let No_change table:make
  set cod_density table:get No_change ticks
end





to create-CodScenario3-table ; This is the scenario where cod population changes slowly
 let Medium_speed_collapse table:make
table:put Medium_speed_collapse 0 0.9
table:put Medium_speed_collapse	1	0.9
table:put Medium_speed_collapse	2	0.96
table:put Medium_speed_collapse	3	0.97
table:put Medium_speed_collapse	4	0.98
table:put Medium_speed_collapse	5	0.99
table:put Medium_speed_collapse	6	1
table:put Medium_speed_collapse	7	1.01
table:put Medium_speed_collapse	8	1.02
table:put Medium_speed_collapse	9	1.03
table:put Medium_speed_collapse	10	1.04
table:put Medium_speed_collapse	11	1.05
table:put Medium_speed_collapse	12	1.06
table:put Medium_speed_collapse	13	1.07
table:put Medium_speed_collapse	14	1.08
table:put Medium_speed_collapse	15	1.09
table:put Medium_speed_collapse	16	1.1
table:put Medium_speed_collapse	17	1.11
table:put Medium_speed_collapse	18	1.12
table:put Medium_speed_collapse	19	1.13
table:put Medium_speed_collapse	20	1.14
table:put Medium_speed_collapse	21	1.15
table:put Medium_speed_collapse	22	1.16
table:put Medium_speed_collapse	23	1.17
table:put Medium_speed_collapse	24	1.18
table:put Medium_speed_collapse	25	1.19
table:put Medium_speed_collapse	26	1.2
table:put Medium_speed_collapse	27	1.21
table:put Medium_speed_collapse	28	1.22
table:put Medium_speed_collapse	29	1.23
table:put Medium_speed_collapse	30	1.24
table:put Medium_speed_collapse	31	1.25
table:put Medium_speed_collapse	32	1.26
table:put Medium_speed_collapse	33	1.27
table:put Medium_speed_collapse	34	1.28
table:put Medium_speed_collapse	35	1.29
table:put Medium_speed_collapse	36	1.3
table:put Medium_speed_collapse	37	1.31
table:put Medium_speed_collapse	38	1.32
table:put Medium_speed_collapse	39	1.33
table:put Medium_speed_collapse	40	1.34
table:put Medium_speed_collapse	41	1.35
table:put Medium_speed_collapse	42	1.36
table:put Medium_speed_collapse	43	1.37
table:put Medium_speed_collapse	44	1.38
table:put Medium_speed_collapse	45	1.39
table:put Medium_speed_collapse	46	1.4
table:put Medium_speed_collapse	47	1.41
table:put Medium_speed_collapse	48	1.42
table:put Medium_speed_collapse	49	1.43
table:put Medium_speed_collapse	50	1.44
table:put Medium_speed_collapse	51	1.45
table:put Medium_speed_collapse	52	1.46
table:put Medium_speed_collapse	53	1.05
table:put Medium_speed_collapse	54	1.061538462
table:put Medium_speed_collapse	55	1.073076923
table:put Medium_speed_collapse	56	1.084615385
table:put Medium_speed_collapse	57	1.096153846
table:put Medium_speed_collapse	58	1.107692308
table:put Medium_speed_collapse	59	1.119230769
table:put Medium_speed_collapse	60	1.130769231
table:put Medium_speed_collapse	61	1.142307692
table:put Medium_speed_collapse	62	1.153846154
table:put Medium_speed_collapse	63	1.165384615
table:put Medium_speed_collapse	64	1.176923077
table:put Medium_speed_collapse	65	1.188461538
table:put Medium_speed_collapse	66	1.2
table:put Medium_speed_collapse	67	1.211538462
table:put Medium_speed_collapse	68	1.223076923
table:put Medium_speed_collapse	69	1.234615385
table:put Medium_speed_collapse	70	1.246153846
table:put Medium_speed_collapse	71	1.257692308
table:put Medium_speed_collapse	72	1.269230769
table:put Medium_speed_collapse	73	1.280769231
table:put Medium_speed_collapse	74	1.292307692
table:put Medium_speed_collapse	75	1.303846154
table:put Medium_speed_collapse	76	1.315384615
table:put Medium_speed_collapse	77	1.326923077
table:put Medium_speed_collapse	78	1.338461538
table:put Medium_speed_collapse	79	1.35
table:put Medium_speed_collapse	80	1.361538462
table:put Medium_speed_collapse	81	1.373076923
table:put Medium_speed_collapse	82	1.384615385
table:put Medium_speed_collapse	83	1.396153846
table:put Medium_speed_collapse	84	1.407692308
table:put Medium_speed_collapse	85	1.419230769
table:put Medium_speed_collapse	86	1.430769231
table:put Medium_speed_collapse	87	1.442307692
table:put Medium_speed_collapse	88	1.453846154
table:put Medium_speed_collapse	89	1.465384615
table:put Medium_speed_collapse	90	1.476923077
table:put Medium_speed_collapse	91	1.488461538
table:put Medium_speed_collapse	92	1.5
table:put Medium_speed_collapse	93	1.511538462
table:put Medium_speed_collapse	94	1.523076923
table:put Medium_speed_collapse	95	1.534615385
table:put Medium_speed_collapse	96	1.546153846
table:put Medium_speed_collapse	97	1.557692308
table:put Medium_speed_collapse	98	1.569230769
table:put Medium_speed_collapse	99	1.580769231
table:put Medium_speed_collapse	100	1.592307692
table:put Medium_speed_collapse	101	1.603846154
table:put Medium_speed_collapse	102	1.615384615
table:put Medium_speed_collapse	103	1.626923077
table:put Medium_speed_collapse	104	1.638461538
table:put Medium_speed_collapse	105	1.65
table:put Medium_speed_collapse	106	1.660576923
table:put Medium_speed_collapse	107	1.671153846
table:put Medium_speed_collapse	108	1.681730769
table:put Medium_speed_collapse	109	1.692307692
table:put Medium_speed_collapse	110	1.702884615
table:put Medium_speed_collapse	111	1.713461538
table:put Medium_speed_collapse	112	1.724038462
table:put Medium_speed_collapse	113	1.734615385
table:put Medium_speed_collapse	114	1.745192308
table:put Medium_speed_collapse	115	1.755769231
table:put Medium_speed_collapse	116	1.766346154
table:put Medium_speed_collapse	117	1.776923077
table:put Medium_speed_collapse	118	1.7875
table:put Medium_speed_collapse	119	1.798076923
table:put Medium_speed_collapse	120	1.808653846
table:put Medium_speed_collapse	121	1.819230769
table:put Medium_speed_collapse	122	1.829807692
table:put Medium_speed_collapse	123	1.840384615
table:put Medium_speed_collapse	124	1.850961538
table:put Medium_speed_collapse	125	1.861538462
table:put Medium_speed_collapse	126	1.872115385
table:put Medium_speed_collapse	127	1.882692308
table:put Medium_speed_collapse	128	1.893269231
table:put Medium_speed_collapse	129	1.903846154
table:put Medium_speed_collapse	130	1.914423077
table:put Medium_speed_collapse	131	1.925
table:put Medium_speed_collapse	132	1.935576923
table:put Medium_speed_collapse	133	1.946153846
table:put Medium_speed_collapse	134	1.956730769
table:put Medium_speed_collapse	135	1.967307692
table:put Medium_speed_collapse	136	1.977884615
table:put Medium_speed_collapse	137	1.988461538
table:put Medium_speed_collapse	138	1.999038462
table:put Medium_speed_collapse	139	2.009615385
table:put Medium_speed_collapse	140	2.020192308
table:put Medium_speed_collapse	141	2.030769231
table:put Medium_speed_collapse	142	2.041346154
table:put Medium_speed_collapse	143	2.051923077
table:put Medium_speed_collapse	144	2.0625
table:put Medium_speed_collapse	145	2.073076923
table:put Medium_speed_collapse	146	2.083653846
table:put Medium_speed_collapse	147	2.094230769
table:put Medium_speed_collapse	148	2.104807692
table:put Medium_speed_collapse	149	2.115384615
table:put Medium_speed_collapse	150	2.125961538
table:put Medium_speed_collapse	151	2.136538462
table:put Medium_speed_collapse	152	2.147115385
table:put Medium_speed_collapse	153	2.157692308
table:put Medium_speed_collapse	154	2.168269231
table:put Medium_speed_collapse	155	2.178846154
table:put Medium_speed_collapse	156	2.189423077
table:put Medium_speed_collapse	157	2.2
table:put Medium_speed_collapse	158	2.197115385
table:put Medium_speed_collapse	159	2.194230769
table:put Medium_speed_collapse	160	2.191346154
table:put Medium_speed_collapse	161	2.188461538
table:put Medium_speed_collapse	162	2.185576923
table:put Medium_speed_collapse	163	2.182692308
table:put Medium_speed_collapse	164	2.179807692
table:put Medium_speed_collapse	165	2.176923077
table:put Medium_speed_collapse	166	2.174038462
table:put Medium_speed_collapse	167	2.171153846
table:put Medium_speed_collapse	168	2.168269231
table:put Medium_speed_collapse	169	2.165384615
table:put Medium_speed_collapse	170	2.1625
table:put Medium_speed_collapse	171	2.159615385
table:put Medium_speed_collapse	172	2.156730769
table:put Medium_speed_collapse	173	2.153846154
table:put Medium_speed_collapse	174	2.150961538
table:put Medium_speed_collapse	175	2.148076923
table:put Medium_speed_collapse	176	2.145192308
table:put Medium_speed_collapse	177	2.142307692
table:put Medium_speed_collapse	178	2.139423077
table:put Medium_speed_collapse	179	2.136538462
table:put Medium_speed_collapse	180	2.133653846
table:put Medium_speed_collapse	181	2.130769231
table:put Medium_speed_collapse	182	2.127884615
table:put Medium_speed_collapse	183	2.125
table:put Medium_speed_collapse	184	2.122115385
table:put Medium_speed_collapse	185	2.119230769
table:put Medium_speed_collapse	186	2.116346154
table:put Medium_speed_collapse	187	2.113461538
table:put Medium_speed_collapse	188	2.110576923
table:put Medium_speed_collapse	189	2.107692308
table:put Medium_speed_collapse	190	2.104807692
table:put Medium_speed_collapse	191	2.101923077
table:put Medium_speed_collapse	192	2.099038462
table:put Medium_speed_collapse	193	2.096153846
table:put Medium_speed_collapse	194	2.093269231
table:put Medium_speed_collapse	195	2.090384615
table:put Medium_speed_collapse	196	2.0875
table:put Medium_speed_collapse	197	2.084615385
table:put Medium_speed_collapse	198	2.081730769
table:put Medium_speed_collapse	199	2.078846154
table:put Medium_speed_collapse	200	2.075961538
table:put Medium_speed_collapse	201	2.073076923
table:put Medium_speed_collapse	202	2.070192308
table:put Medium_speed_collapse	203	2.067307692
table:put Medium_speed_collapse	204	2.064423077
table:put Medium_speed_collapse	205	2.061538462
table:put Medium_speed_collapse	206	2.058653846
table:put Medium_speed_collapse	207	2.055769231
table:put Medium_speed_collapse	208	2.052884615
table:put Medium_speed_collapse	209	2.05
table:put Medium_speed_collapse	210	2.046153846
table:put Medium_speed_collapse	211	2.042307692
table:put Medium_speed_collapse	212	2.038461538
table:put Medium_speed_collapse	213	2.034615385
table:put Medium_speed_collapse	214	2.030769231
table:put Medium_speed_collapse	215	2.026923077
table:put Medium_speed_collapse	216	2.023076923
table:put Medium_speed_collapse	217	2.019230769
table:put Medium_speed_collapse	218	2.015384615
table:put Medium_speed_collapse	219	2.011538462
table:put Medium_speed_collapse	220	2.007692308
table:put Medium_speed_collapse	221	2.003846154
table:put Medium_speed_collapse	222	2
table:put Medium_speed_collapse	223	1.996153846
table:put Medium_speed_collapse	224	1.992307692
table:put Medium_speed_collapse	225	1.988461538
table:put Medium_speed_collapse	226	1.984615385
table:put Medium_speed_collapse	227	1.980769231
table:put Medium_speed_collapse	228	1.976923077
table:put Medium_speed_collapse	229	1.973076923
table:put Medium_speed_collapse	230	1.969230769
table:put Medium_speed_collapse	231	1.965384615
table:put Medium_speed_collapse	232	1.961538462
table:put Medium_speed_collapse	233	1.957692308
table:put Medium_speed_collapse	234	1.953846154
table:put Medium_speed_collapse	235	1.95
table:put Medium_speed_collapse	236	1.946153846
table:put Medium_speed_collapse	237	1.942307692
table:put Medium_speed_collapse	238	1.938461538
table:put Medium_speed_collapse	239	1.934615385
table:put Medium_speed_collapse	240	1.930769231
table:put Medium_speed_collapse	241	1.926923077
table:put Medium_speed_collapse	242	1.923076923
table:put Medium_speed_collapse	243	1.919230769
table:put Medium_speed_collapse	244	1.915384615
table:put Medium_speed_collapse	245	1.911538462
table:put Medium_speed_collapse	246	1.907692308
table:put Medium_speed_collapse	247	1.903846154
table:put Medium_speed_collapse	248	1.9
table:put Medium_speed_collapse	249	1.896153846
table:put Medium_speed_collapse	250	1.892307692
table:put Medium_speed_collapse	251	1.888461538
table:put Medium_speed_collapse	252	1.884615385
table:put Medium_speed_collapse	253	1.880769231
table:put Medium_speed_collapse	254	1.876923077
table:put Medium_speed_collapse	255	1.873076923
table:put Medium_speed_collapse	256	1.869230769
table:put Medium_speed_collapse	257	1.865384615
table:put Medium_speed_collapse	258	1.861538462
table:put Medium_speed_collapse	259	1.857692308
table:put Medium_speed_collapse	260	1.853846154
table:put Medium_speed_collapse	261	1.85
table:put Medium_speed_collapse	262	1.851923077
table:put Medium_speed_collapse	263	1.853846154
table:put Medium_speed_collapse	264	1.855769231
table:put Medium_speed_collapse	265	1.857692308
table:put Medium_speed_collapse	266	1.859615385
table:put Medium_speed_collapse	267	1.861538462
table:put Medium_speed_collapse	268	1.863461538
table:put Medium_speed_collapse	269	1.865384615
table:put Medium_speed_collapse	270	1.867307692
table:put Medium_speed_collapse	271	1.869230769
table:put Medium_speed_collapse	272	1.871153846
table:put Medium_speed_collapse	273	1.873076923
table:put Medium_speed_collapse	274	1.875
table:put Medium_speed_collapse	275	1.876923077
table:put Medium_speed_collapse	276	1.878846154
table:put Medium_speed_collapse	277	1.880769231
table:put Medium_speed_collapse	278	1.882692308
table:put Medium_speed_collapse	279	1.884615385
table:put Medium_speed_collapse	280	1.886538462
table:put Medium_speed_collapse	281	1.888461538
table:put Medium_speed_collapse	282	1.890384615
table:put Medium_speed_collapse	283	1.892307692
table:put Medium_speed_collapse	284	1.894230769
table:put Medium_speed_collapse	285	1.896153846
table:put Medium_speed_collapse	286	1.898076923
table:put Medium_speed_collapse	287	1.9
table:put Medium_speed_collapse	288	1.901923077
table:put Medium_speed_collapse	289	1.903846154
table:put Medium_speed_collapse	290	1.905769231
table:put Medium_speed_collapse	291	1.907692308
table:put Medium_speed_collapse	292	1.909615385
table:put Medium_speed_collapse	293	1.911538462
table:put Medium_speed_collapse	294	1.913461538
table:put Medium_speed_collapse	295	1.915384615
table:put Medium_speed_collapse	296	1.917307692
table:put Medium_speed_collapse	297	1.919230769
table:put Medium_speed_collapse	298	1.921153846
table:put Medium_speed_collapse	299	1.923076923
table:put Medium_speed_collapse	300	1.925
  table:put Medium_speed_collapse	301	1.926923077
table:put Medium_speed_collapse	302	1.928846154
table:put Medium_speed_collapse	303	1.930769231
table:put Medium_speed_collapse	304	1.932692308
table:put Medium_speed_collapse	305	1.934615385
table:put Medium_speed_collapse	306	1.936538462
table:put Medium_speed_collapse	307	1.938461538
table:put Medium_speed_collapse	308	1.940384615
table:put Medium_speed_collapse	309	1.942307692
table:put Medium_speed_collapse	310	1.944230769
table:put Medium_speed_collapse	311	1.946153846
table:put Medium_speed_collapse	312	1.948076923
table:put Medium_speed_collapse	313	1.95
table:put Medium_speed_collapse	314	1.950961538
table:put Medium_speed_collapse	315	1.951923077
table:put Medium_speed_collapse	316	1.952884615
table:put Medium_speed_collapse	317	1.953846154
table:put Medium_speed_collapse	318	1.954807692
table:put Medium_speed_collapse	319	1.955769231
table:put Medium_speed_collapse	320	1.956730769
table:put Medium_speed_collapse	321	1.957692308
table:put Medium_speed_collapse	322	1.958653846
table:put Medium_speed_collapse	323	1.959615385
table:put Medium_speed_collapse	324	1.960576923
table:put Medium_speed_collapse	325	1.961538462
table:put Medium_speed_collapse	326	1.9625
table:put Medium_speed_collapse	327	1.963461538
table:put Medium_speed_collapse	328	1.964423077
table:put Medium_speed_collapse	329	1.965384615
table:put Medium_speed_collapse	330	1.966346154
table:put Medium_speed_collapse	331	1.967307692
table:put Medium_speed_collapse	332	1.968269231
table:put Medium_speed_collapse	333	1.969230769
table:put Medium_speed_collapse	334	1.970192308
table:put Medium_speed_collapse	335	1.971153846
table:put Medium_speed_collapse	336	1.972115385
table:put Medium_speed_collapse	337	1.973076923
table:put Medium_speed_collapse	338	1.974038462
table:put Medium_speed_collapse	339	1.975
table:put Medium_speed_collapse	340	1.975961538
table:put Medium_speed_collapse	341	1.976923077
table:put Medium_speed_collapse	342	1.977884615
table:put Medium_speed_collapse	343	1.978846154
table:put Medium_speed_collapse	344	1.979807692
table:put Medium_speed_collapse	345	1.980769231
table:put Medium_speed_collapse	346	1.981730769
table:put Medium_speed_collapse	347	1.982692308
table:put Medium_speed_collapse	348	1.983653846
table:put Medium_speed_collapse	349	1.984615385
table:put Medium_speed_collapse	350	1.985576923
table:put Medium_speed_collapse	351	1.986538462
table:put Medium_speed_collapse	352	1.9875
table:put Medium_speed_collapse	353	1.988461538
table:put Medium_speed_collapse	354	1.989423077
table:put Medium_speed_collapse	355	1.990384615
table:put Medium_speed_collapse	356	1.991346154
table:put Medium_speed_collapse	357	1.992307692
table:put Medium_speed_collapse	358	1.993269231
table:put Medium_speed_collapse	359	1.994230769
table:put Medium_speed_collapse	360	1.995192308
table:put Medium_speed_collapse	361	1.996153846
table:put Medium_speed_collapse	362	1.997115385
table:put Medium_speed_collapse	363	1.998076923
table:put Medium_speed_collapse	364	1.999038462
table:put Medium_speed_collapse	365	2
table:put Medium_speed_collapse	366	1.996153846
table:put Medium_speed_collapse	367	1.992307692
table:put Medium_speed_collapse	368	1.988461538
table:put Medium_speed_collapse	369	1.984615385
table:put Medium_speed_collapse	370	1.980769231
table:put Medium_speed_collapse	371	1.976923077
table:put Medium_speed_collapse	372	1.973076923
table:put Medium_speed_collapse	373	1.969230769
table:put Medium_speed_collapse	374	1.965384615
table:put Medium_speed_collapse	375	1.961538462
table:put Medium_speed_collapse	376	1.957692308
table:put Medium_speed_collapse	377	1.953846154
table:put Medium_speed_collapse	378	1.95
table:put Medium_speed_collapse	379	1.946153846
table:put Medium_speed_collapse	380	1.942307692
table:put Medium_speed_collapse	381	1.938461538
table:put Medium_speed_collapse	382	1.934615385
table:put Medium_speed_collapse	383	1.930769231
table:put Medium_speed_collapse	384	1.926923077
table:put Medium_speed_collapse	385	1.923076923
table:put Medium_speed_collapse	386	1.919230769
table:put Medium_speed_collapse	387	1.915384615
table:put Medium_speed_collapse	388	1.911538462
table:put Medium_speed_collapse	389	1.907692308
table:put Medium_speed_collapse	390	1.903846154
table:put Medium_speed_collapse	391	1.9
table:put Medium_speed_collapse	392	1.896153846
table:put Medium_speed_collapse	393	1.892307692
table:put Medium_speed_collapse	394	1.888461538
table:put Medium_speed_collapse	395	1.884615385
table:put Medium_speed_collapse	396	1.880769231
table:put Medium_speed_collapse	397	1.876923077
table:put Medium_speed_collapse	398	1.873076923
table:put Medium_speed_collapse	399	1.869230769
table:put Medium_speed_collapse	400	1.865384615
table:put Medium_speed_collapse	401	1.861538462
table:put Medium_speed_collapse	402	1.857692308
table:put Medium_speed_collapse	403	1.853846154
table:put Medium_speed_collapse	404	1.85
table:put Medium_speed_collapse	405	1.846153846
table:put Medium_speed_collapse	406	1.842307692
table:put Medium_speed_collapse	407	1.838461538
table:put Medium_speed_collapse	408	1.834615385
table:put Medium_speed_collapse	409	1.830769231
table:put Medium_speed_collapse	410	1.826923077
table:put Medium_speed_collapse	411	1.823076923
table:put Medium_speed_collapse	412	1.819230769
table:put Medium_speed_collapse	413	1.815384615
table:put Medium_speed_collapse	414	1.811538462
table:put Medium_speed_collapse	415	1.807692308
table:put Medium_speed_collapse	416	1.803846154
table:put Medium_speed_collapse	417	1.8
table:put Medium_speed_collapse	418	1.786538462
table:put Medium_speed_collapse	419	1.773076923
table:put Medium_speed_collapse	420	1.759615385
table:put Medium_speed_collapse	421	1.746153846
table:put Medium_speed_collapse	422	1.732692308
table:put Medium_speed_collapse	423	1.719230769
table:put Medium_speed_collapse	424	1.705769231
table:put Medium_speed_collapse	425	1.692307692
table:put Medium_speed_collapse	426	1.678846154
table:put Medium_speed_collapse	427	1.665384615
table:put Medium_speed_collapse	428	1.651923077
table:put Medium_speed_collapse	429	1.638461538
table:put Medium_speed_collapse	430	1.625
table:put Medium_speed_collapse	431	1.611538462
table:put Medium_speed_collapse	432	1.598076923
table:put Medium_speed_collapse	433	1.584615385
table:put Medium_speed_collapse	434	1.571153846
table:put Medium_speed_collapse	435	1.557692308
table:put Medium_speed_collapse	436	1.544230769
table:put Medium_speed_collapse	437	1.530769231
table:put Medium_speed_collapse	438	1.517307692
table:put Medium_speed_collapse	439	1.503846154
table:put Medium_speed_collapse	440	1.490384615
table:put Medium_speed_collapse	441	1.476923077
table:put Medium_speed_collapse	442	1.463461538
table:put Medium_speed_collapse	443	1.45
table:put Medium_speed_collapse	444	1.436538462
table:put Medium_speed_collapse	445	1.423076923
table:put Medium_speed_collapse	446	1.409615385
table:put Medium_speed_collapse	447	1.396153846
table:put Medium_speed_collapse	448	1.382692308
table:put Medium_speed_collapse	449	1.369230769
table:put Medium_speed_collapse	450	1.355769231
table:put Medium_speed_collapse	451	1.342307692
table:put Medium_speed_collapse	452	1.328846154
table:put Medium_speed_collapse	453	1.315384615
table:put Medium_speed_collapse	454	1.301923077
table:put Medium_speed_collapse	455	1.288461538
table:put Medium_speed_collapse	456	1.275
table:put Medium_speed_collapse	457	1.261538462
table:put Medium_speed_collapse	458	1.248076923
table:put Medium_speed_collapse	459	1.234615385
table:put Medium_speed_collapse	460	1.221153846
table:put Medium_speed_collapse	461	1.207692308
table:put Medium_speed_collapse	462	1.194230769
table:put Medium_speed_collapse	463	1.180769231
table:put Medium_speed_collapse	464	1.167307692
table:put Medium_speed_collapse	465	1.153846154
table:put Medium_speed_collapse	466	1.140384615
table:put Medium_speed_collapse	467	1.126923077
table:put Medium_speed_collapse	468	1.113461538
table:put Medium_speed_collapse	469	1.1
table:put Medium_speed_collapse	470	1.096153846
table:put Medium_speed_collapse	471	1.092307692
table:put Medium_speed_collapse	472	1.088461538
table:put Medium_speed_collapse	473	1.084615385
table:put Medium_speed_collapse	474	1.080769231
table:put Medium_speed_collapse	475	1.076923077
table:put Medium_speed_collapse	476	1.073076923
table:put Medium_speed_collapse	477	1.069230769
table:put Medium_speed_collapse	478	1.065384615
table:put Medium_speed_collapse	479	1.061538462
table:put Medium_speed_collapse	480	1.057692308
table:put Medium_speed_collapse	481	1.053846154
table:put Medium_speed_collapse	482	1.05
table:put Medium_speed_collapse	483	1.046153846
table:put Medium_speed_collapse	484	1.042307692
table:put Medium_speed_collapse	485	1.038461538
table:put Medium_speed_collapse	486	1.034615385
table:put Medium_speed_collapse	487	1.030769231
table:put Medium_speed_collapse	488	1.026923077
table:put Medium_speed_collapse	489	1.023076923
table:put Medium_speed_collapse	490	1.019230769
table:put Medium_speed_collapse	491	1.015384615
table:put Medium_speed_collapse	492	1.011538462
table:put Medium_speed_collapse	493	1.007692308
table:put Medium_speed_collapse	494	1.003846154
table:put Medium_speed_collapse	495	1
table:put Medium_speed_collapse	496	0.996153846
table:put Medium_speed_collapse	497	0.992307692
table:put Medium_speed_collapse	498	0.988461538
table:put Medium_speed_collapse	499	0.984615385
table:put Medium_speed_collapse	500	0.980769231
table:put Medium_speed_collapse	501	0.976923077
table:put Medium_speed_collapse	502	0.973076923
table:put Medium_speed_collapse	503	0.969230769
table:put Medium_speed_collapse	504	0.965384615
table:put Medium_speed_collapse	505	0.961538462
table:put Medium_speed_collapse	506	0.957692308
table:put Medium_speed_collapse	507	0.953846154
table:put Medium_speed_collapse	508	0.95
table:put Medium_speed_collapse	509	0.946153846
table:put Medium_speed_collapse	510	0.942307692
table:put Medium_speed_collapse	511	0.938461538
table:put Medium_speed_collapse	512	0.934615385
table:put Medium_speed_collapse	513	0.930769231
table:put Medium_speed_collapse	514	0.926923077
table:put Medium_speed_collapse	515	0.923076923
table:put Medium_speed_collapse	516	0.919230769
table:put Medium_speed_collapse	517	0.915384615
table:put Medium_speed_collapse	518	0.911538462
table:put Medium_speed_collapse	519	0.907692308
table:put Medium_speed_collapse	520	0.903846154
table:put Medium_speed_collapse	521	0.9
table:put Medium_speed_collapse	522	0.898076923
table:put Medium_speed_collapse	523	0.896153846
table:put Medium_speed_collapse	524	0.894230769
table:put Medium_speed_collapse	525	0.892307692
table:put Medium_speed_collapse	526	0.890384615
table:put Medium_speed_collapse	527	0.888461538
table:put Medium_speed_collapse	528	0.886538462
table:put Medium_speed_collapse	529	0.884615385
table:put Medium_speed_collapse	530	0.882692308
table:put Medium_speed_collapse	531	0.880769231
table:put Medium_speed_collapse	532	0.878846154
table:put Medium_speed_collapse	533	0.876923077
table:put Medium_speed_collapse	534	0.875
table:put Medium_speed_collapse	535	0.873076923
table:put Medium_speed_collapse	536	0.871153846
table:put Medium_speed_collapse	537	0.869230769
table:put Medium_speed_collapse	538	0.867307692
table:put Medium_speed_collapse	539	0.865384615
table:put Medium_speed_collapse	540	0.863461538
table:put Medium_speed_collapse	541	0.861538462
table:put Medium_speed_collapse	542	0.859615385
table:put Medium_speed_collapse	543	0.857692308
table:put Medium_speed_collapse	544	0.855769231
table:put Medium_speed_collapse	545	0.853846154
table:put Medium_speed_collapse	546	0.851923077
table:put Medium_speed_collapse	547	0.85
table:put Medium_speed_collapse	548	0.848076923
table:put Medium_speed_collapse	549	0.846153846
table:put Medium_speed_collapse	550	0.844230769
table:put Medium_speed_collapse	551	0.842307692
table:put Medium_speed_collapse	552	0.840384615
table:put Medium_speed_collapse	553	0.838461538
table:put Medium_speed_collapse	554	0.836538462
table:put Medium_speed_collapse	555	0.834615385
table:put Medium_speed_collapse	556	0.832692308
table:put Medium_speed_collapse	557	0.830769231
table:put Medium_speed_collapse	558	0.828846154
table:put Medium_speed_collapse	559	0.826923077
table:put Medium_speed_collapse	560	0.825
table:put Medium_speed_collapse	561	0.823076923
table:put Medium_speed_collapse	562	0.821153846
table:put Medium_speed_collapse	563	0.819230769
table:put Medium_speed_collapse	564	0.817307692
table:put Medium_speed_collapse	565	0.815384615
table:put Medium_speed_collapse	566	0.813461538
table:put Medium_speed_collapse	567	0.811538462
table:put Medium_speed_collapse	568	0.809615385
table:put Medium_speed_collapse	569	0.807692308
table:put Medium_speed_collapse	570	0.805769231
table:put Medium_speed_collapse	571	0.803846154
table:put Medium_speed_collapse	572	0.801923077
table:put Medium_speed_collapse	573	0.8
table:put Medium_speed_collapse	574	0.797115385
table:put Medium_speed_collapse	575	0.794230769
table:put Medium_speed_collapse	576	0.791346154
table:put Medium_speed_collapse	577	0.788461538
table:put Medium_speed_collapse	578	0.785576923
table:put Medium_speed_collapse	579	0.782692308
table:put Medium_speed_collapse	580	0.779807692
table:put Medium_speed_collapse	581	0.776923077
table:put Medium_speed_collapse	582	0.774038462
table:put Medium_speed_collapse	583	0.771153846
table:put Medium_speed_collapse	584	0.768269231
table:put Medium_speed_collapse	585	0.765384615
table:put Medium_speed_collapse	586	0.7625
table:put Medium_speed_collapse	587	0.759615385
table:put Medium_speed_collapse	588	0.756730769
table:put Medium_speed_collapse	589	0.753846154
table:put Medium_speed_collapse	590	0.750961538
table:put Medium_speed_collapse	591	0.748076923
table:put Medium_speed_collapse	592	0.745192308
table:put Medium_speed_collapse	593	0.742307692
table:put Medium_speed_collapse	594	0.739423077
table:put Medium_speed_collapse	595	0.736538462
table:put Medium_speed_collapse	596	0.733653846
table:put Medium_speed_collapse	597	0.730769231
table:put Medium_speed_collapse	598	0.727884615
table:put Medium_speed_collapse	599	0.725
table:put Medium_speed_collapse	600	0.722115385
table:put Medium_speed_collapse	601	0.719230769
table:put Medium_speed_collapse	602	0.716346154
table:put Medium_speed_collapse	603	0.713461538
table:put Medium_speed_collapse	604	0.710576923
table:put Medium_speed_collapse	605	0.707692308
table:put Medium_speed_collapse	606	0.704807692
table:put Medium_speed_collapse	607	0.701923077
table:put Medium_speed_collapse	608	0.699038462
table:put Medium_speed_collapse	609	0.696153846
table:put Medium_speed_collapse	610	0.693269231
table:put Medium_speed_collapse	611	0.690384615
table:put Medium_speed_collapse	612	0.6875
table:put Medium_speed_collapse	613	0.684615385
table:put Medium_speed_collapse	614	0.681730769
table:put Medium_speed_collapse	615	0.678846154
table:put Medium_speed_collapse	616	0.675961538
table:put Medium_speed_collapse	617	0.673076923
table:put Medium_speed_collapse	618	0.670192308
table:put Medium_speed_collapse	619	0.667307692
table:put Medium_speed_collapse	620	0.664423077
table:put Medium_speed_collapse	621	0.661538462
table:put Medium_speed_collapse	622	0.658653846
table:put Medium_speed_collapse	623	0.655769231
table:put Medium_speed_collapse	624	0.652884615
table:put Medium_speed_collapse	625	0.65
table:put Medium_speed_collapse	626	0.647115385
table:put Medium_speed_collapse	627	0.644230769
table:put Medium_speed_collapse	628	0.641346154
table:put Medium_speed_collapse	629	0.638461538
table:put Medium_speed_collapse	630	0.635576923
table:put Medium_speed_collapse	631	0.632692308
table:put Medium_speed_collapse	632	0.629807692
table:put Medium_speed_collapse	633	0.626923077
table:put Medium_speed_collapse	634	0.624038462
table:put Medium_speed_collapse	635	0.621153846
table:put Medium_speed_collapse	636	0.618269231
table:put Medium_speed_collapse	637	0.615384615
table:put Medium_speed_collapse	638	0.6125
table:put Medium_speed_collapse	639	0.609615385
table:put Medium_speed_collapse	640	0.606730769
table:put Medium_speed_collapse	641	0.603846154
table:put Medium_speed_collapse	642	0.600961538
table:put Medium_speed_collapse	643	0.598076923
table:put Medium_speed_collapse	644	0.595192308
table:put Medium_speed_collapse	645	0.592307692
table:put Medium_speed_collapse	646	0.589423077
table:put Medium_speed_collapse	647	0.586538462
table:put Medium_speed_collapse	648	0.583653846
table:put Medium_speed_collapse	649	0.580769231
table:put Medium_speed_collapse	650	0.577884615
table:put Medium_speed_collapse	651	0.575
table:put Medium_speed_collapse	652	0.572115385
table:put Medium_speed_collapse	653	0.569230769
table:put Medium_speed_collapse	654	0.566346154
table:put Medium_speed_collapse	655	0.563461538
table:put Medium_speed_collapse	656	0.560576923
table:put Medium_speed_collapse	657	0.557692308
table:put Medium_speed_collapse	658	0.554807692
table:put Medium_speed_collapse	659	0.551923077
table:put Medium_speed_collapse	660	0.549038462
table:put Medium_speed_collapse	661	0.546153846
table:put Medium_speed_collapse	662	0.543269231
table:put Medium_speed_collapse	663	0.540384615
table:put Medium_speed_collapse	664	0.5375
table:put Medium_speed_collapse	665	0.534615385
table:put Medium_speed_collapse	666	0.531730769
table:put Medium_speed_collapse	667	0.528846154
table:put Medium_speed_collapse	668	0.525961538
table:put Medium_speed_collapse	669	0.523076923
table:put Medium_speed_collapse	670	0.520192308
table:put Medium_speed_collapse	671	0.517307692
table:put Medium_speed_collapse	672	0.514423077
table:put Medium_speed_collapse	673	0.511538462
table:put Medium_speed_collapse	674	0.508653846
table:put Medium_speed_collapse	675	0.505769231
table:put Medium_speed_collapse	676	0.502884615


  set cod_density table:get Medium_speed_collapse ticks
end
@#$#@#$#@
GRAPHICS-WINDOW
417
12
926
522
-1
-1
15.2
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

SLIDER
26
426
148
459
rewire.prob
rewire.prob
0
0.99
0.25
0.01
1
NIL
HORIZONTAL

BUTTON
27
26
116
59
NIL
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

BUTTON
28
72
115
105
NIL
ca
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
26
466
148
499
neighborhood.size
neighborhood.size
0
8
2.0
1
1
NIL
HORIZONTAL

SLIDER
123
64
253
97
SI_threshold
SI_threshold
0
1
0.6
.01
1
NIL
HORIZONTAL

PLOT
949
14
1630
134
Cod density
ticks
cod density
0.0
70.0
0.0
4.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot cod_density"
"pen-1" 1.0 2 -11221820 true "" "plot msy_p"

CHOOSER
25
265
131
310
Cod.scenario
Cod.scenario
"Observed_scenario" "Logistic_growth" "linear_growth"
0

BUTTON
27
116
115
149
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
1485
13
1631
58
Cod density at each tick
cod_density
17
1
11

MONITOR
798
532
927
577
Current switched fishers
fishers_just_switched
17
1
11

BUTTON
27
158
116
191
tick once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
123
100
253
133
Sd_SI
Sd_SI
0.0
1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
123
26
394
59
tick_span
tick_span
0
3000
676.0
1
1
NIL
HORIZONTAL

CHOOSER
158
466
285
511
tie.strength.distribution
tie.strength.distribution
"random" "random_normal" "random_poisson" "random_exponential"
1

CHOOSER
305
588
397
633
layout
layout
"radial" "spring" "circle" "tutte"
2

PLOT
683
639
929
764
Degree distribution
Degrees
Nb nodes
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [ count my-links ] of turtles"

BUTTON
305
513
395
547
Layout
layout-turtles
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
305
549
395
582
Layout once
layout-turtles
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
158
426
397
459
fishers
fishers
1
200
100.0
1
1
NIL
HORIZONTAL

SLIDER
292
466
396
499
full.time.fishers
full.time.fishers
0
20
0.0
1
1
NIL
HORIZONTAL

MONITOR
530
586
630
631
Average centrality
average-centrality
4
1
11

MONITOR
637
585
730
630
Number of edges
number-of-edges
17
1
11

MONITOR
418
587
528
632
NIL
number-of-non-isolates
4
1
11

MONITOR
735
584
856
629
Average shortest path
average-shortest-path
4
1
11

SLIDER
148
264
396
297
carrying.capacity
carrying.capacity
100
70000
43334.0
100
1
NIL
HORIZONTAL

SLIDER
26
370
131
403
log.rate
log.rate
0
.3
0.09
.005
1
NIL
HORIZONTAL

PLOT
949
338
1634
458
Fish population size
Amount of fish
Ticks
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot cod_population"
"pen-1" 1.0 0 -11221820 true "" "plot msy"

PLOT
949
469
1632
589
Fish population increase
Ticks
Recruited fish
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot regenAmount"

PLOT
949
600
1634
764
Fishing effort
Ticks
Amount of fish caught
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot extractAmount"
"pen-1" 1.0 0 -2674135 true "" "plot msfe"

SLIDER
123
136
253
169
slope_SI
slope_SI
0
200
150.0
5
1
NIL
HORIZONTAL

SWITCH
123
176
253
209
SI_in
SI_in
0
1
-1000

MONITOR
678
532
793
577
Total switched fishers
total_op_fish_switched
17
1
11

CHOOSER
28
545
147
590
flow.type
flow.type
"direct" "indirect"
0

SLIDER
149
299
396
332
t0.fish.population
t0.fish.population
1000
25000
21889.0
100
1
NIL
HORIZONTAL

MONITOR
417
532
523
577
background_fishers
other_fishers
17
1
11

MONITOR
527
533
587
578
all_fishers
all_fishers
17
1
11

SLIDER
152
372
396
405
background.effort
background.effort
0
1000
900.0
1
1
NIL
HORIZONTAL

MONITOR
1436
417
1599
462
Cod population at each tick
cod_population
2
1
11

SWITCH
150
336
259
369
fish.mortality
fish.mortality
1
1
-1000

SWITCH
28
506
147
539
info.flow
info.flow
1
1
-1000

SLIDER
25
320
130
353
linear.rate
linear.rate
0
2
1.0
0.1
1
NIL
HORIZONTAL

MONITOR
590
533
676
578
Back switchers
proportion_of_switched_back_fishers
0
1
11

MONITOR
862
470
919
515
switchers
switched_fishers_per_tick
17
1
11

MONITOR
864
585
927
630
# back switchers
back_switchers_per_tick
17
1
11

SLIDER
260
65
394
98
EI_threshold
EI_threshold
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
260
99
394
132
Sd_EI
Sd_EI
0
1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
23
202
115
235
thr-exit
thr-exit
0
.8
0.3
0.01
1
NIL
HORIZONTAL

SWITCH
262
176
395
209
EI_in
EI_in
0
1
-1000

SLIDER
262
136
394
169
slope_EI
slope_EI
1
200
150.0
1
1
NIL
HORIZONTAL

SWITCH
262
213
396
246
EI_out
EI_out
0
1
-1000

PLOT
419
640
674
763
Threshold distribution
Thresholds
fishers
0.0
10.0
0.0
20.0
true
false
"" ""
PENS
"T_SI" 1.0 0 -2674135 true "" "histogram [threshold_SI * 10 ] of turtles with [ livelihood_strategy = \"opportunistic_fisher\"]"
"T_EI" 1.0 0 -13345367 true "" "histogram [threshold_EI * 10 ] of turtles with [ livelihood_strategy = \"opportunistic_fisher\"]"
"T_SI + T_EI" 1.0 0 -16777216 true "" "histogram [combined_threshold * 10 ] of turtles with [ livelihood_strategy = \"opportunistic_fisher\"]"

MONITOR
1428
14
1485
59
MaxCD
mcd
17
1
11

MONITOR
1583
463
1640
508
MaxF
mf
17
1
11

MONITOR
1582
337
1639
382
MSY
msy
17
1
11

MONITOR
425
467
499
512
Transitivity
global-clustering
17
1
11

PLOT
949
168
1633
330
Percentage of fishers switched & proportional fish density 
Ticks
% of switched fishers
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -5825686 true "" "plot (cod_density * 20)"
"pen-2" 1.0 0 -15456499 true "" "plot active_opp_fishers"
"pen-3" 1.0 0 -2674135 true "" "plot msfe_p"

MONITOR
1582
159
1639
204
MSFE
msfe
3
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

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

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
  <experiment name="experiment- demo" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Number_opportunistic_fishers">
      <value value="46"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rewire_probability">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Neighborhood_size">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Peer_effects">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Opportunity_threshold">
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mean_propensity_to_switch">
      <value value="61"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory_decay">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number_fishers">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="20.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Slow_collapse2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population_size">
      <value value="142"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Peer vs. no peer" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <metric>monitor_neigh</metric>
    <enumeratedValueSet variable="Number_opportunistic_fishers">
      <value value="40"/>
      <value value="30"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rewire_probability">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Neighborhood_size">
      <value value="5"/>
      <value value="3"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Peer_effects">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="22.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mean_propensity_to_switch">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Memory_decay">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number_fishers">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Opportunity_threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Fast_collapse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population_size">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment - 11 mayo 2021" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <enumeratedValueSet variable="Number_opportunistic_fishers">
      <value value="100"/>
      <value value="95"/>
      <value value="90"/>
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rewire_probability">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Neighborhood_size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Opportunity_threshold">
      <value value="1.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Slow_collapse2&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="interdependence">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population_size">
      <value value="101"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heterogeneity effects - observed" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repulsion-constant">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contribution_of_weighted_influence">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number_opportunistic_fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spring-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rewire_probability">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.15"/>
      <value value="0.1"/>
      <value value="0.05"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Neighborhood_size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spring-constant">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="133"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population_size">
      <value value="101"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-realization">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-rows">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-cols">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="131"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contribution_of_weighted_influence">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number_opportunistic_fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_strength_distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;radial&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-exponent">
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_tie_strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population_size">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_threshold">
      <value value="0.77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;weighted&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrap">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;core-periphery-200&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_tie_strength">
      <value value="0.7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment- test ro sreb" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="20"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="small-world-probabilistic-test" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <metric>cod_density</metric>
    <metric>average-centrality</metric>
    <metric>number-of-edges</metric>
    <metric>number-of-non-isolates</metric>
    <metric>global-clustering</metric>
    <metric>average-shortest-path</metric>
    <metric>seed-centrality</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_strength_distribution">
      <value value="1"/>
      <value value="10"/>
      <value value="25"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_tie_strength">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="preferential-attachment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <metric>cod_density</metric>
    <metric>average-centrality</metric>
    <metric>number-of-edges</metric>
    <metric>number-of-non-isolates</metric>
    <metric>global-clustering</metric>
    <metric>average-shortest-path</metric>
    <metric>seed-centrality</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;preferential attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="core-periphery" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <metric>cod_density</metric>
    <metric>average-centrality</metric>
    <metric>number-of-edges</metric>
    <metric>number-of-non-isolates</metric>
    <metric>global-clustering</metric>
    <metric>average-shortest-path</metric>
    <metric>seed-centrality</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="network-realization" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;core-periphery-50&quot;"/>
      <value value="&quot;core-periphery-100&quot;"/>
      <value value="&quot;core-periphery-150&quot;"/>
      <value value="&quot;core-periphery-200&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-realization">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-rows">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-cols">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contribution_of_weighted_influence">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_strength_distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-exponent">
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_tie_strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_threshold">
      <value value="0.77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrap">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_tie_strength">
      <value value="0.7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="random" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <metric>cod_density</metric>
    <metric>average-centrality</metric>
    <metric>number-of-edges</metric>
    <metric>number-of-non-isolates</metric>
    <metric>global-clustering</metric>
    <metric>average-shortest-path</metric>
    <metric>seed-centrality</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="50"/>
      <value value="100"/>
      <value value="150"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connection-prob">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="two_communities" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <metric>cod_density</metric>
    <metric>average-centrality</metric>
    <metric>number-of-edges</metric>
    <metric>number-of-non-isolates</metric>
    <metric>global-clustering</metric>
    <metric>average-shortest-path</metric>
    <metric>seed-centrality</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="network-realization" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;2communities50&quot;"/>
      <value value="&quot;2communities100&quot;"/>
      <value value="&quot;2communities150&quot;"/>
      <value value="&quot;2communities200&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="three_communities" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>proportion_of_fishers_switched</metric>
    <metric>cod_density</metric>
    <metric>average-centrality</metric>
    <metric>number-of-edges</metric>
    <metric>number-of-non-isolates</metric>
    <metric>global-clustering</metric>
    <metric>average-shortest-path</metric>
    <metric>seed-centrality</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="network-realization" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="memory_decay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_threshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_threshold">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbor_influence">
      <value value="&quot;additive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_propensity_to_switch">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;3communities50&quot;"/>
      <value value="&quot;3communities100&quot;"/>
      <value value="&quot;3communities150&quot;"/>
      <value value="&quot;3communities200&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_of_neighbor_effects">
      <value value="0"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_opportunity_cost_threshold">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Setting M_26-04" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>switched_fishers_per_tick</metric>
    <metric>proportion_of_switched_back_fishers</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="switch_back">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear_reg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_strength_distribution">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-exponent">
      <value value="5.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_tie_strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information_flow">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info-flow">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
      <value value="70"/>
      <value value="110"/>
      <value value="160"/>
      <value value="220"/>
      <value value="290"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fx_of_fishing">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_threshold">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_op_cost_threshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_tie_strength">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Setting M for EI_11-05" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>switched_fishers_per_tick</metric>
    <metric>back_switchers_per_tick</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="switch_back">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear_reg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_strength_distribution">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_tie_strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information_flow">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info-flow">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
      <value value="70"/>
      <value value="110"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fx_of_fishing">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_threshold">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_tie_strength">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="switch_back">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Describing EI 11-05-2022" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>cod_density</metric>
    <metric>active_opp_fishers</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="switch_back">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear_reg">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_Exp_Inc">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects_back">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Exp.Inc_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information_flow">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info-flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fx_of_fishing">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <steppedValueSet variable="expected_income" first="0.1" step="0.2" last="0.9"/>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjuster">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_threshold">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Describing EI &amp; EI_Sd 11-05-2022" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>cod_density</metric>
    <metric>active_opp_fishers</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="switch_back">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear_reg">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_Exp_Inc">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects_back">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Exp.Inc_effects">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information_flow">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info-flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fx_of_fishing">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expected_income">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjuster">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_base_value_of_threshold">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Describing EI &amp; EI_Sd + SI 11-05-2022" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>cod_density</metric>
    <metric>active_opp_fishers</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exiting">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear_reg">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects_back">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information_flow">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info-flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fx_of_fishing">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjuster">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighbour_effects">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sd_EI &amp; Sd_SI" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>cod_density</metric>
    <metric>active_opp_fishers</metric>
    <enumeratedValueSet variable="seeds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nb-nodes">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear_reg">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie_strength_distribution">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links-to-use">
      <value value="&quot;undirected&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_tie_strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exiting">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information_flow">
      <value value="&quot;indirect&quot;"/>
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-before-generating?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info-flow">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fx_of_fishing">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod_scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjuster">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;small world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_tie_strength">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Statistical analysis" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>cod_density</metric>
    <metric>active_opp_fishers</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;indirect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Sd_SI" first="0" step="0.08" last="0.32"/>
    <steppedValueSet variable="Sd_EI" first="0" step="0.08" last="0.32"/>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean.tie.strength">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full.time.fishers">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="SI_threshold" first="0.17" step="0.17" last="0.85"/>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="101"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="thr-exit" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="Sd.tie.strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Setting M for SI" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>switched_fishers_per_tick</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean.tie.strength">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full.time.fishers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd.tie.strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="99"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EI + EI_Sd + Si 0.5" repetitions="30" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>msfe_p</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Logistic_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.05"/>
      <value value="0.15"/>
      <value value="0.25"/>
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="400"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment2 - Social influence &amp; thr-exit" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>msfe_p</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Logistic_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full.time.fishers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.15"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.15"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment 3 - threxit + SI on &amp; off" repetitions="30" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>msfe_p</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Logistic_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full.time.fishers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.15"/>
      <value value="0.23"/>
      <value value="0.24"/>
      <value value="0.25"/>
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd.tie.strength">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A3" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.05"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A4" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;linear_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment B1" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>msfe_p</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Logistic_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.05"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="2000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment B2" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>msfe_p</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Logistic_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0"/>
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="2000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment B3" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>msfe_p</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Logistic_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.05"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="2000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment B4" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>msfe_p</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Logistic_growth&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="2000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment C1 - SSF" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>cod_density</metric>
    <metric>msfe_p</metric>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full.time.fishers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment C1 - T" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>cod_density</metric>
    <metric>msfe_p</metric>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full.time.fishers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment C1 - X" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>active_opp_fishers</metric>
    <metric>cod_density</metric>
    <enumeratedValueSet variable="SI_threshold">
      <value value="0.25"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full.time.fishers">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t0.fish.population">
      <value value="21889"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="background.effort">
      <value value="900"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fish.mortality">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SI_in">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carrying.capacity">
      <value value="43334"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_SI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire.prob">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flow.type">
      <value value="&quot;direct&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="linear.rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_out">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope_EI">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log.rate">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Cod.scenario">
      <value value="&quot;Observed_scenario&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_SI">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fishers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood.size">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="thr-exit">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sd_EI">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick_span">
      <value value="676"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tie.strength.distribution">
      <value value="&quot;random_normal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="EI_threshold">
      <value value="0.5"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="info.flow">
      <value value="false"/>
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
0
@#$#@#$#@
