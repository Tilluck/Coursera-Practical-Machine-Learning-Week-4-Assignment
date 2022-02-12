<body>


<div class="container-fluid main-container">




<div id="header">



<h1 class="title toc-ignore">Coursera Practical Machine Learning Week-4 Assignment</h1>
<h4 class="author">Shawn</h4>
<h4 class="date">2/11/2022</h4>

</div>


<div id="introduction" class="section level2">
<h2>Introduction</h2>
<p>Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <a href="http://groupware.les.inf.puc-rio.br/har" class="uri">http://groupware.les.inf.puc-rio.br/har</a> (see the section on the Weight Lifting Exercise Dataset).</p>
<p>The training data for this project are available here:</p>
<p><a href="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" class="uri">https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv</a></p>
<p>The test data are available here:</p>
<p><a href="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" class="uri">https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv</a></p>
<pre class="r"><code>library(caret)</code></pre>
<pre><code>## Warning: package &#39;caret&#39; was built under R version 4.1.2</code></pre>
<pre><code>## Loading required package: ggplot2</code></pre>
<pre><code>## Warning: package &#39;ggplot2&#39; was built under R version 4.1.2</code></pre>
<pre><code>## Loading required package: lattice</code></pre>
<pre class="r"><code>library(rpart)</code></pre>
<pre><code>## Warning: package &#39;rpart&#39; was built under R version 4.1.2</code></pre>
<pre class="r"><code>library(rpart.plot)</code></pre>
<pre><code>## Warning: package &#39;rpart.plot&#39; was built under R version 4.1.2</code></pre>
<pre class="r"><code>library(rattle)</code></pre>
<pre><code>## Warning: package &#39;rattle&#39; was built under R version 4.1.2</code></pre>
<pre><code>## Loading required package: tibble</code></pre>
<pre><code>## Warning: package &#39;tibble&#39; was built under R version 4.1.2</code></pre>
<pre><code>## Loading required package: bitops</code></pre>
<pre><code>## Rattle: A free graphical interface for data science with R.
## Version 5.4.0 Copyright (c) 2006-2020 Togaware Pty Ltd.
## Type &#39;rattle()&#39; to shake, rattle, and roll your data.</code></pre>
<pre class="r"><code>library(randomForest)</code></pre>
<pre><code>## Warning: package &#39;randomForest&#39; was built under R version 4.1.2</code></pre>
<pre><code>## randomForest 4.7-1</code></pre>
<pre><code>## Type rfNews() to see new features/changes/bug fixes.</code></pre>
<pre><code>## 
## Attaching package: &#39;randomForest&#39;</code></pre>
<pre><code>## The following object is masked from &#39;package:rattle&#39;:
## 
##     importance</code></pre>
<pre><code>## The following object is masked from &#39;package:ggplot2&#39;:
## 
##     margin</code></pre>
<pre class="r"><code>library(gbm)</code></pre>
<pre><code>## Warning: package &#39;gbm&#39; was built under R version 4.1.2</code></pre>
<pre><code>## Loaded gbm 2.1.8</code></pre>
</div>
<div id="reading-csvs" class="section level1">
<h1>Reading CSVs</h1>
<pre class="r"><code>training_set&lt;- read.csv(&quot;pml-training.csv&quot;)
testing_set&lt;- read.csv(&quot;pml-testing.csv&quot;)</code></pre>
</div>
<div id="cleaning-the-data" class="section level1">
<h1>Cleaning the Data</h1>
<pre class="r"><code>nearZeroVar &lt;- nearZeroVar(training_set)
train_data &lt;- training_set[,-nearZeroVar]
test_data &lt;- testing_set[,-nearZeroVar]
str(test_data)</code></pre>
<pre class="r"><code>NaCols &lt;- sapply(train_data, function(x) mean(is.na(x))) &gt; 0.95
train_data &lt;- train_data[,NaCols == FALSE]
test_data &lt;- test_data[,NaCols == FALSE]
str(test_data)</code></pre>
</div>
<div id="removing-the-first-6-non-numeric-variables" class="section level1">
<h1>Removing the first 6 non-numeric variables</h1>
<pre class="r"><code>train_data &lt;- train_data[,7:59]
test_data &lt;- test_data[,7:59]
str(test_data)</code></pre>
</div>
<div id="creating-testing-and-a-validation-set-6040-split" class="section level1">
<h1>Creating testing and a validation set 60/40 split</h1>
<pre class="r"><code>inTrain&lt;- createDataPartition(train_data$classe, p=0.6, list=FALSE)
training&lt;- train_data[inTrain,]
validating&lt;- train_data[-inTrain,]
dim(training)</code></pre>
<pre><code>## [1] 11776    53</code></pre>
<pre class="r"><code>dim(validating)</code></pre>
<pre><code>## [1] 7846   53</code></pre>
</div>
<div id="runing-a-decision-tree-model" class="section level1">
<h1>Runing a Decision Tree model</h1>
<pre class="r"><code>DT_modelfit&lt;- train(classe ~. , data=training, method= &quot;rpart&quot;)
fancyRpartPlot(DT_modelfit$finalModel)</code></pre>
<p><img src="data:image/png;base64,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" /><!-- --></p>
<pre class="r"><code>DT_prediction &lt;- predict(DT_modelfit, validating)
confusionMatrix(as.factor(DT_prediction), as.factor(validating$classe))</code></pre>
<pre><code>## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2023  610  627  575  205
##          B   34  534   48  228  198
##          C  168  374  693  483  408
##          D    0    0    0    0    0
##          E    7    0    0    0  631
## 
## Overall Statistics
##                                           
##                Accuracy : 0.4946          
##                  95% CI : (0.4835, 0.5058)
##     No Information Rate : 0.2845          
##     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
##                                           
##                   Kappa : 0.34            
##                                           
##  Mcnemar&#39;s Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9064  0.35178  0.50658   0.0000  0.43759
## Specificity            0.6407  0.91972  0.77879   1.0000  0.99891
## Pos Pred Value         0.5007  0.51248  0.32596      NaN  0.98903
## Neg Pred Value         0.9451  0.85538  0.88199   0.8361  0.88749
## Prevalence             0.2845  0.19347  0.17436   0.1639  0.18379
## Detection Rate         0.2578  0.06806  0.08833   0.0000  0.08042
## Detection Prevalence   0.5149  0.13281  0.27097   0.0000  0.08132
## Balanced Accuracy      0.7735  0.63575  0.64268   0.5000  0.71825</code></pre>
<p>The Decision Tree Model has a low accuracy level</p>
</div>
<div id="running-a-random-forest-model" class="section level1">
<h1>Running a Random Forest Model</h1>
<pre class="r"><code>RF_modelfit &lt;- train(classe ~ ., data = training, method = &quot;rf&quot;, ntree = 100)
RF_prediction&lt;- predict(RF_modelfit, validating)
qplot(RF_prediction,validating$classe, colour=validating$classe)</code></pre>
<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAqAAAAHgCAMAAABNUi8GAAAA21BMVEUAAAAAADoAAGYAOpAAZrYAsPYAv30zMzM6AAA6ADo6AGY6OpA6kNtNTU1NTW5NTY5NbqtNjshmAABmADpmOpBmZmZmtv9uTU1uTW5uTY5ubqtuq+SOTU2OTW6ObquOjo6OyP+QOgCQOjqQOmaQkDqQkGaQtpCQ27aQ2/+jpQCrbk2rbm6rbo6r5Mir5P+2ZgC22/+2///Ijk3I///bkDrb/7bb/9vb///kq27k///na/Pr6+vy8vL4dm3/tmb/yI7/25D/27b/29v/5Kv//7b//8j//9v//+T///86krQfAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAYwUlEQVR4nO2dC1sb2ZmExRhfxgsZD9kwGU+cmOzOxJfExDa7vuwayIKh//8v2j6t1gVz1JI+UaeqW/U+MwhJqFyc89I3taRRZYwwI3YBY7qwoEYaC2qksaBGGgtqpLGgRprVBf2fIOEHDjpcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoXpEPTip/2a371pr5YerWGHSxYvYtyadAn68/v5q6VHaxU+f/7cz3DN4mjZIvRa0M+fgRONDBctjpYtwmqCPqwp0WZNmqnoY3hvi5dn6Tbok0/t1egftJegZbO3cgmaiI6Xt0ELZ3sbdM3fOvrAQYdLFkfLFsGCcsIli6Nli7B0G3T/eXu19GgNO1yyeBHj1sTPJHHCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoVZLOjFT/s1z6fXS4/WsMMli5cQbl06BP35fVV9/dPU0NKjRQ+/f/9+L7Pj4UWMW5MlglYXTz+116PD1VdB79/HWYTM3iC8iHFrskzQr8/e1F8f1hSrJEIzzz3MxoYXZ5mg16/ftNejf9BegpbN3solaCI6Xn0V1NugEiwT9OzJtm6Dei9eAu/Fc8Ilixcxbk18HJQTLlm8hHDr4meSOOGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxiGGnexs93IJywiWLr+TB+fcv6v+ab0+/ezu9+V//rCY3z3N5MBrd+zD/4JWFG2NBOeGSxVfyYM6xOUEXmHd1tHu6dzwz1IL2JVyy+EoerCVoffPp3tx9FrQv4ZLFs/N+dZQ2Ik++e3v+aDQa7U1W8fW6e+fXWtDxrenrXnPPb/V3h82qfee3x/VjDsfboFdHo9HuWNA2Z3x5OLtMPzK3zWBBqeGSxfMTf1KvoWtLLw8Ox542gl4e7NUSfvd27tZG3Uf1D5+km8d3V8ejP+w1fu5W6UebR84eUct5OLlMP9L8WxZUIVyyeH7iW/f+70M12UNKK+60rKs1m7t1LOjheL3e3l1/aRaPk1X7fM7j8dJyctk8pLHXggqESxbPT3xaxzdLttPatZ1W0OaGxq3prdP9+xt3V/VO0s6LydZqc//4EfXCNa30q8llMnm87regAuGSxRfM/Om9/z5Ka+ydF7Ml6MTAuVsXClobPifo5BFVs6Ha3N5c3l67W1BiuGTxBTN/+ft6d6fWNC1Ed+ZX8fWXuVvnBZ3eXf9XC3o4t4qfPGIc3a7S68vJTRZUIlyy+KKpP07r4CTQ+aOJoJcHu81e0OTWdg9oIuhsH2rvdK/eFk17QOn/JGj7iMbhicv15dVRbe5tS28KWm8HHC5Y1FrQ3mTfuaCn6ShQ2lbc+dvB4TeHmdpbk8Rzgk7vTs8kJQPnDjNNHtFuik4v04/cXoreEPT43n8djHf3LSg4XLL4IkGDNCvzu3wuvl5MpyX16e2jpYnSozXscMniG5l0g7SqXrSkWw8LygmXLH4HPk1IB43uws+bq/iTtIpP27c5So/WsMMli9+FUHfNzZ2k09yh0pbSozXscMniBXxbGx9m4oRLFgd6FsaCcsIliwM9C3ND0GYvKXPKU0Pp0Rp2uGTxvCL/m+fOVcxz8zjobjoD5cTHQfHhksXziugIejk+Su/DTAXCJYvnFZESNB1jsqAFwiWL5xXRETSdFpXO0/MqHh8uWTyviI6g6dUhu9WxTxYpEC5ZPK+IkKCdlB6tYYdLFs9PvAXdgN6GSxbPT7yQoD4OWixcsnheESFBfRy0WLhk8bwiywS9/P2LO9PxNj4OygmXLJ5XZObkly9fMoKe/mGzU5K78XFQTrhk8bwi837OGTq5++qvf/v3/HGfO8HHQTnhksXziiwR9PzfPhx/+24Ld4iPg3LCJYvnFVki6Em9zr2Tc+fz+DATJ1yyeH7iu7dB02sxFx34uQssKCdcsnh+4rv34us1fFUB1/HfruJHC/8eSo/WsMMli+cV6Rb0JLkJXMd/s5N0dXR4+w3GxpQerWGHSxbPK9Ip6NV/NG+uhDsU+s1hpup4r329/S1Kj9awwyWL5xXpXoKi+VbQEx+oLxIuWTyviI6g6Qjo6cL3wSs9WsMOlyyeV0RI0PRWpceZN3BqKD1aww6XLJ5XREjQTkqP1rDDJYvnJ96CbkBvwyWL5ydeRND0Ro4t3knCh0sWzysiIuhSSo/WsMMli+cn3oJuQG/DJYvnJ15I0LQXv/BtR0uP1rDDJYvnJ15I0OZMUL8FeIlwyeJ5RXQEbZ+F9zNJBcIli+cVsaAb0NtwyeJ5RXQEHX+4ot8CvES4ZPG8IkKC+i3Ai4VLFs9PfLeg43OIC52w3Enp0Rp2uGTx/MTPnHz58uVtQR+/raYfZgzAgnLCJYvnJ37ezzlD23sbQa/++mJjExcwJ2h6OWd6wtOr+ALhksXzE7+CoM0LkzDMBD2u95Cujg69k1QkXLJ4XpElgjbboAteqn4HzJ0sstceYfIJywXCJYvnFVlhGzQt2EBMBT0ZzZH750qP1rDDJYvnFVmyF58EBb7ueG4JetgeqfcStEC4ZPG8IisIWuRVnfU+UnOg/vyRt0Hx4ZLF84osEbTYcdCro/bj5b0XXyBcsnh+4rsFRePjoJxwyeL5idcRdPaqj9wJd6VHa9jhksXziugIOj1ZpD0l9Gx/f//Jp8mdpUdr2OGSxfOK6Ag6O92uOR768Yf3VfVuamjp0VqFBw8e9DMcWnxLBP367E195frV8/be0qO1Ag8eACcaGQ4tPlxBZ+eDpkOhZ7O1e/WwplChdWjmuY/h0OJ3jZCg4/NBD6uTdLzp7MebP1n6z3kFvATNM9gl6A0unn66cb30aK2Ct0GzbIegPdgG7XG4ZPG8CEKC3nwL8B7sxfc3XLJ4XhEdQb99RbyPg/Yze7CCLnp3+jGlR2vY4ZLF8xOvI2j3aaelR2vY4ZLF8xOvI+iit2wYU3q0hh0uWTw/8csEPS3zko9qdrKI31kEHy5ZPK/IzMkHDx7cFrR5dmfR52dujk+344RLFs9P/Lyfc4a2945Ppse9KMmCcsIli+cnvlvQBZ+qdWfMvybJq/hy4ZLF84osERT4SccJL0E54ZLF8xPfvQ0KfM+GBr/9Iidcsnheke69+JLboBa0XLhk8bwiSw4zFduLn71zg98CHB8uWTyvyBJBix4H9TNJpcIli+cnfpmgWLyTxAmXLJ6feCFBb55u9w2lR2vY4ZLF84roCHp1tFfvjS1a0ZcerWGHSxbPK6IjaFLzeG/RcwOlR2vY4ZLF84poCXqy68NMJcIli+cV0RE0fdJcbafffrFAuGTxvCJCgqbP6jxu3uMuQ+nRGna4ZPG8IkKCdlJ6tIYdLlk8P/EWdAN6Gy5ZPD/xIoLO3nvRx0ELhEsWzysiImiifW8mHwfFh0sWzyuiI6jPZioXLlk8r4gF3YDehksWzyuiI6g/jrtcuGTxvCLdgpb9tOPTjn+s9GgNO1yyeH7iZ07ev3//tqCPO/Zb7gAfZuKESxbPT/y8n3OGtveOP2kO98okC8oJlyyen/hVBMV91JxfdswJlyyeV2QVQXEfGO8lKCdcsnh+4lfYBi2xBF1K6dEadrhk8fzEL9mLbwTFvb+In+rkhEsWzyuygqDeix9cuGTx/MQvEbTocdAuSo/WsMMli+cnvltQNH5VJydcsnheER1B/arOcuGSxfOK6AjqV3WWC5csnldES1C/qrNMuGTxvCI6gvpVneXCJYvnFRES1K/qLBYuWTyviJCgnZQerWGHSxbPT7wF3YDehksWz0+8jqCLzqUfU3q0hh0uWTw/8TqCNm+yvPBJq9KjNexwyeL5iRcStOZ45J2kEuGSxfOKaAlac+zjoPhwyeJ5RbQEPV70VLwF7U32cAVdvH6vLGh/sgmn2y3WZlP8KR+ccMni+YmfOfn58+fbgj7u+Aj3O8DHQTnhksXzEz/v55yh7b0WdJjhksXzE29BN6C34ZLF8xO/RNBmG7TMJ811U3q0hh0uWTw/8d4G3YDehksWz0/8kr14CzrIcMni+Ym3oBvQ23DJ4vmJXyLo+IWWfl38wMIli+cnvltQNBaUEy5ZPD/xFnQDehsuWTw/8RZ0A3obLlk8P/EWdAN6Gy5ZPD/xFnQDehsuWTw/8RZ0A3obLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LEyHoBc/7dc8n1wtPVrDDpcsXkK4dekS9Of39Zc/vmmvBn/ply9fBh855HBo8a0S9Pr1ZoK+fAmci96GQ4tvl6AXTz/V3z6sieU3UxF76IDDocWHxdJt0Cef2quxP8reLuS8BNVg6Sr+1WQvKfhL93Yz0dugEiwTtHq3oaCa+6v0cMniJYRbl2WCfn224V685lTQwyWLFzFuTXwclBMuWbyEcOviZ5I44ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CdAv69dmb6felR2vY4ZLF0bJF6Bb07C+/TL8P/tJfvnwJPnLQ4UhBw8XRskXoFPT69T/+/GlyJTxYuHnubzhS0HhxvG7r0ynoxdNP756nbx7WxPKb0Yo9dMjhSHpbPEunoB9/qc5+nFwp/ec86HAvQVelS9DrV/v7+z+8b6/Fhyv4yEGHext0RboErdfwVTVex1fei+9P9tbsxX9Mbk7X8aVHa9jhksUL+LY2HYJe/z2t3aeHQkuP1rDDJYsXMW5N/EwSJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LIwF5YRLFgd6FsaCcsIliwM9C2NBOeGSxYGehbGgnHDJ4kDPwlhQTrhkcaBnYSwoJ1yyONCzMBaUEy5ZHOhZGAvKCZcsDvQsjAXlhEsWB3oWxoJywiWLAz0LY0E54ZLFgZ6FsaCccMniQM/CWFBOuGRxoGdhLCgnXLI40LMwFpQTLlkc6FkYC8oJlywO9CyMBeWESxYHehbGgnLCJYsDPQtjQTnhksWBnoWxoJxwyeJAz8JYUE64ZHGgZ2EsKCdcsjjQszAWlBMuWRzoWRgLygmXLA70LMzqgkZ56PDC2djwwlhQTnhvi5fGgnLCe1u8NBaUE97b4qXBC2rMBlhQI40FNdJYUCONBTXSoAX9+uwNKvrip/2a56D0szr7ySdEMrb3OP13sFEvDVrQs7/8goq++Pl9/eWPmKn4+EMd/g5iaNP7659AhjbpAwIs6PXrf/wZshiq2qm4fg0RdLzgv36FsGis0MVTzLhY0LWop+EdbGXWLEEx83yGWbs3jBVCbftY0LX4+Et19iMoe7y1hTEJVrqaKARa9ENHhQFW0OtX9WD9APqTHq/iIWth2Aq4yfYSdA2wgjbzjFrHj6cCk47fBkVtRFjQdfiYJhi1uhzvDYMWRN6LFwEq6PXfoSszHwddmA6LL46fSTLSWFAjjQU10lhQI40FNdJYUCPNdgl6dTRq2Jt9e7j8UaffvT3//sX06r/+Wc1fNVC2TdC9dHH+6HDy7SrUgs5ds5xF2UpBq+NdC9oTLOiE8+9/HY3ufajOH//n6Lu3aQsgeXl5MNr5dbyKTzft1kvfegthdrV+2G+PVtpQMCG2UtDTUWYVf/4oaZkM3E0/WH85uffh8mCvdrQRNN10eXDYLEHbq+Mfr6U+ubGMNXfItgk63TNqv51ZmjZMk3nN5el46XnYXJ40gk5W7a2gzT1p96l9GOcXGj7bJuheMnGvqnKr+BeNk83lSbu3Xy9E63sev50YWc0Ebe5pvbWgMLZQ0GYNv0zQpF/VXlpQItsoaHVcu5bZBj2cuFg7vPOiuXG6Ir+9ik8/MTlCakFhbKWgaT9o8U5Sku3qqF5A1g5eHuzO7yTN7ShNd5IsKJStFLTexNzLH2banciW9qHSMvLWYaZ6+TvavXGY6YUFBbJdgnZhySSxoBMsqCRbL2h6Ziix85sFVWTrBTXaWFAjjQU10lhQI40FNdJYUCONBTXS/D/JYwUB6ROLTwAAAABJRU5ErkJggg==" /><!-- --></p>
<pre class="r"><code>RF_confusionMatrix&lt;-confusionMatrix(as.factor(RF_prediction), as.factor(validating$classe))
RF_confusionMatrix</code></pre>
<pre><code>## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2229    9    0    0    0
##          B    1 1506    7    0    2
##          C    1    2 1355   18    6
##          D    0    1    6 1267    7
##          E    1    0    0    1 1427
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9921          
##                  95% CI : (0.9899, 0.9939)
##     No Information Rate : 0.2845          
##     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
##                                           
##                   Kappa : 0.99            
##                                           
##  Mcnemar&#39;s Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9987   0.9921   0.9905   0.9852   0.9896
## Specificity            0.9984   0.9984   0.9958   0.9979   0.9997
## Pos Pred Value         0.9960   0.9934   0.9805   0.9891   0.9986
## Neg Pred Value         0.9995   0.9981   0.9980   0.9971   0.9977
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2841   0.1919   0.1727   0.1615   0.1819
## Detection Prevalence   0.2852   0.1932   0.1761   0.1633   0.1821
## Balanced Accuracy      0.9985   0.9953   0.9932   0.9915   0.9946</code></pre>
<p>The Random Forest Model accuracy is 99%</p>
</div>
<div id="running-a-gradient-boosting-model" class="section level1">
<h1>Running a Gradient Boosting Model</h1>
<pre class="r"><code>gbm_modelfit&lt;- train(classe~., data=training, method=&quot;gbm&quot;, verbose= FALSE)
gbm_prediction&lt;- predict(gbm_modelfit, validating)
qplot(gbm_prediction,validating$classe, colour=validating$classe)</code></pre>
<p><img src="data:image/png;base64,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" /><!-- --></p>
<pre class="r"><code>gbm_confusionMatrix&lt;-confusionMatrix(as.factor(gbm_prediction), as.factor(validating$classe))
gbm_confusionMatrix</code></pre>
<pre><code>## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2207   49    0    2    2
##          B   12 1424   37    3   25
##          C    8   44 1311   44   13
##          D    4    0   17 1225   20
##          E    1    1    3   12 1382
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9621          
##                  95% CI : (0.9577, 0.9663)
##     No Information Rate : 0.2845          
##     P-Value [Acc &gt; NIR] : &lt; 2.2e-16       
##                                           
##                   Kappa : 0.9521          
##                                           
##  Mcnemar&#39;s Test P-Value : 1.618e-12       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9888   0.9381   0.9583   0.9526   0.9584
## Specificity            0.9906   0.9878   0.9832   0.9938   0.9973
## Pos Pred Value         0.9765   0.9487   0.9232   0.9676   0.9878
## Neg Pred Value         0.9955   0.9852   0.9911   0.9907   0.9907
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2813   0.1815   0.1671   0.1561   0.1761
## Detection Prevalence   0.2880   0.1913   0.1810   0.1614   0.1783
## Balanced Accuracy      0.9897   0.9630   0.9708   0.9732   0.9779</code></pre>
<p>The Gradient Boosting Model accuracy is 96%</p>
</div>
<div id="conclusion" class="section level1">
<h1>Conclusion</h1>
<p>The Random Forest model is more accurate than Gradient Boosting Model at ~ 99% accuracy. Expected out-of-sample error = 1 - accuracy of cross-validation testing = 0.01</p>
<p>#Running The Random Forest model on the test data</p>
<pre class="r"><code>test_prediction&lt;- predict(RF_modelfit, test_data)
test_prediction</code></pre>
<pre><code>##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E</code></pre>
</div>




</div>

<script>

// add bootstrap table styles to pandoc tables
function bootstrapStylePandocTables() {
  $('tr.odd').parent('tbody').parent('table').addClass('table table-condensed');
}
$(document).ready(function () {
  bootstrapStylePandocTables();
});


</script>

<!-- tabsets -->

<script>
$(document).ready(function () {
  window.buildTabsets("TOC");
});

$(document).ready(function () {
  $('.tabset-dropdown > .nav-tabs > li').click(function () {
    $(this).parent().toggleClass('nav-tabs-open');
  });
});
</script>

<!-- code folding -->


<!-- dynamically load mathjax for compatibility with self-contained -->
<script>
  (function () {
    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src  = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
    document.getElementsByTagName("head")[0].appendChild(script);
  })();
</script>

</body>
