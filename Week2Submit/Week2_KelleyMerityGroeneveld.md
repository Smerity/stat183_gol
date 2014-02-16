<head>
  <style type="text/css">
    body, blockquote{
      margin-left:20px;
      margin-right:20px; 
      font-size:14pt; 
      font-family:Calibri,Arial;
    }
    div.title {
      margin-left:auto;
      margin-right:auto;
      text-align:center; 
      line-spacing:10px;
      font-weight:900; 
      font-size:20pt;
    }
    div.pattern-example {
      line-spacing:5px; 
      letter-spacing:10px; 
      font-weight:900;
    }
    div#three-col { 
      width:300px; 
    }
    div#col1 {
      position:relative; 
      float:left; 
      width:60px; 
      display:table-column;
    }
    div#col2 {
      position:relative; 
      float:left; 
      padding-left:10px;
      padding-top:20px;
      vertical-align:middle; 
      width:150px; 
      display:table-column;
    }
    div#col3 {
      position:relative; 
      float:left; 
      width:60px; 
      display:table-column;
    }
    div.intro {
      margin-left:50px;
      margin-right:50px;
      font-size:16pt;
    }
    .big-indent { margin-left:60px;}
    .text-line { clear:both;}
    span.crimson, a { color:maroon;}
    span.standout { color:blue;}
    span.ltgray { color:gray;}
    span.section-head {
      margin-top:20px; 
      color:maroon; 
      font-size:20pt; 
      font-weight:900;
    }
    img {
      padding-top: 15px; 
      padding-bottom: 15px;
    }
    li {margin-top:10px; line-spacing:15px;}
    code {font-size: 12pt;}
  </style>
</head>

<div class="title">
  STAT183 Challenge:  <span class="crimson">Reverse Game of Life</span>
                      <span class="ltgray"> [week 2]</span> <br />
  <img src="http://schools-wikipedia.org/images/194/19479.png" /> <br />
  Strategy: <span class="crimson">"Running analysis on 5x5 patterns, using memoizing techniques"</span> <br />
  Team: <span class="crimson">Groeneveld, Merity, Randell</span> 
 <br /><br /></div>

Kaggle Game of Life competition
<br>

========================================================
<br>
<div class="intro"><b>Previously, on reversing Conway's Game of Life</b>:<br> 
We used an optimal combination of two benchmark heuristics: <b>start = end</b> and <b>all off</b>. To briefly explain, the <b>start = end</b> heuristic is as simple as letting the startboard equal the end board configuration. The <b>all off</b> heuristic is simply setting all 400 cells in the startboard to off. We established <b>thresholds</b> for each delta case. The threshold dictated the number of live cells for which if there are fewer we would apply the <b>start = end</b> heuristic, if there were a greater number we would just apply the <b>all off</b> heuristic. We realized there was a ceiling dictating how well we could optimize this option, so we began down some different routes. 
</div>

<div class="intro"><b>Reversing Conway's Game of Life</b>:<br> 
<hr>

<span class="section-head">1. CONSTRAINT SATISFACTION APPROACH</span>
<br />
We began this week by approaching it from a Constraint Satisfaction Problem angle. Our idea was that if we could generate <b> all possible starting boards</b> given our end board configuration, then we could overlay all of them and for each cell we would have P(on|start boards) and P(off|start boards) = 1-P(on|start boards). We knew this would be very computationally intensive but we thought if we could at least get it working for delta==1 we would solve that case with very low error. Alas, it turned out to be too computationally intensive... no matter what language we through at it. 
<br />

<span class="section-head">2. 5x5 SUB-BOARD APPROACH - training the model</span>
<br />
For each delta {1,2,3,4,5} we developed the following procedure for training our model.
[include snippet of data generation and splitting by delta]



We took each 5x5 sub-board in our stop board configuration and mapped it as the key to a dictionary. The value of this key was the number of times this sub-pattern in the end board resulted in an alive/on middle pixel in the start board. 
<br />
For example consider our end board 5x5 sub-pattern looked something like this:
  <div class="pattern-example big-indent">
     00000 <br />
     0<span class="standout">11</span>00 <br />
     00<span class="standout">1</span>00 <br />
     00<span class="standout">1</span>00 <br />
     00000 <br /><br />
  </div>
Which has the unique identifying string: 0000001100001000010000000.
<br />
And our starting 5x5 sub-pattern looked something like this:
  <div class="pattern-example big-indent">
     00000 <br />
     0<span class="standout">111</span>0 <br />
     00<span class="standout">0</span>00 <br />
     00000 <br />
     00000 <br /><br />
  </div>

<br /> 
Then we would say that in this instance the pattern 0000001100001000010000000 mapped to a 0 in the middle positioned cell.

<br />

We now have a ditionary of 2^25 5x5 patterns and the percentage of times each pattern had an alive middle cell in the starting configuration, dependent on the delta. 
<br />
[include snippet mapping pattern to dictionary]


<br />


Now to further train our model we utilized a concept known as forward error. After training the model we would apply it to more training data. After generating our prediction for the start board, we run the simulation forward with the number of timesteps equal to the given delta. 
<br />
[include snippet of generating a prediction based on the whole stop board configuration and the dictionary of 5x5 patterns]


<br /><br />
<span class="section-head">3. Computing the forward error</span>
<br />
The forward error is defined to be the error in the prediction board after having evolved delta timesteps to a stop board configuration. After thorough investigation, we found that the forward error was generally one of two extremes: it was either really small error or really large error. When the forward error was really small the actual error on the starting configuration was also small, and the same followed for when the forward error was large. 
<br />
[include snippet of calculating forward error]







</div>






















