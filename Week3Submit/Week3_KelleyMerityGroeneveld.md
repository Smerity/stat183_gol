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
  Strategy: <span class="crimson">"Brute force with forward error ensemble"</span> <br />
  Team: <span class="crimson">Groeneveld, Merity, Randell</span> 
 <br /><br /></div>

Kaggle Game of Life competition
<br>

========================================================
<br>
<div class="intro"><b>Previously, on reversing Conway's Game of Life</b>:<br> 
We further developed our model of forward error and integrated it into our solution to help better choose an optimal solution board. We also went down the pathway of training our model on 5x5 sub-boards of our actual boards and recognizing common patterns. Ultimately we ran out of time as the deadline approached, so we had to submit the solution we had from the previous week.
</div>

<hr>

<span class="section-head">1. Ensemble method with forward error proxy</span>
<br />
We initially started this week by writing an ensemble method with most of the submission files from the previous week. Initial investigation into the correlation of forward error and actual prediction error yeilded about a 0.94 correlation, for all delta cases. We ran our ensemble and in our test submission on Friday it didn't perform as well as we had hoped. It only did a little bit better than the average of all our solution sets. In theory, we thought, we would do as worst as the best solution. So we looked into the correlation more, and noticed that for delta is 1 and 2 the correlation was about 0.96, but as delta increased the correlation went down to 0.90. We concluded perhaps this ensemble method would perform well for detla = 1,2 but not as well as delta grows. Unfortunately the nature of this ensemble method was such that we couldn't test how well we would do before submitting. Essentially we were access the power of the best team's algorithms through their solution sets, but we couldn't repoduce their algorithms on new data that we knew the solution for.
<br />
The general idea of the ensemble method is as follows: for every board in the test data, compute the forward error for every solution set we have and take the board with the least forward error as our new best prediction. As per the above discussion we ended up only running this on delta is 1 and 2, and just used the best solution's board for delta is 1,2 and 3. 
<br/>
We started by loading in all solution sets we wanted to iterate over. In order to reproduce our code you will need files of the following name in your directory. The file names are simple the overall error computed by Kaggle. They are ordered from best to worst total Kaggle prediction error.
<hr>
<code>BEGIN CODE</code>


```r
setwd("~/Documents/SeniorSpring/Stat183/GameofLife/submissions/Reece_Andrew_BryanTonyAndrew/")
source("still-life-functions.R")
```

```
## Loading required package: bitops
```

<br />

```r
# df_test <- read.csv('test.csv') # Load in our predictions to ensemble over
# num_ensemble <- 5 df_1 <- read.csv('11568.csv') df_2 <-
# read.csv('11623.csv') df_3 <- read.csv('11720.csv') df_4 <-
# read.csv('11777.csv') df_5 <- read.csv('12121.csv') # df_best will be by
# default our best prediction, we will update in place if we find a better #
# prediction as by forward error df_best <- df_1 lst =
# list(df_1,df_2,df_3,df_4,df_5,df_6,df_7)

```

<br />
For every board in every solution set, we start by pulling out the test board and the delta from the actual test data. We then proceed to let the default 'best board' equal the board given by the best solution. We then checked if the delta was in our range of valid deltas. If now we skip straight to letting the current default board be the best board prediction. If it was in the range we would then evolve the current solutions sets' board in question delta times. Checked if the forward error was less than the current minimum forward error, if so updated our best board prediction. Our entire best solution set prediction would live in the variable df_best.
<br />

```r
for (i in 1:50000) {
    test_board <- df_test[i, ]
    delta <- test_board[2]
    
    # Get rid of the delta column for the test board, for when we sum/calculate
    # our forward error
    test_board["delta"] <- NULL
    
    # Just let the initial best board be the board in the first place df to
    # initialize best_board.. need here for scope
    best_board <- lst[[1]][i, ][2:401]
    if (delta <= 2) {
        min_err <- 400
        
        # Loop through all predictions for each board -- num_ensemble is the number
        # of total prediction files we have
        for (j in 1:num_ensemble) {
            # Pull out start board prediction -- q_board==question board
            q_board <- lst[[j]][i, ][1:401]
            pred <- matrix(as.integer(q_board[2:401]), nrow = 20, ncol = 20)
            
            # Evolve board forward delta steps in order to calculate forward error
            k = 0
            while (k < delta) {
                pred <- update_board(pred)
                k <- k + 1
            }
            
            # Now board is evolved, check the forward error
            error <- sum(test_board != pred)
            
            if (error < min_err) {
                min_err <- error
                best_board <- q_board
            }
        }
    }
    tot_err <- tot_err + min_err
    df_best[i, ] <- best_board
}
```

```
## Error: object 'df_test' not found
```


<span class="section-head">2. 5x5 and 3x3 SUB-BOARD approach - training the model</span>
<br />


</div>






















