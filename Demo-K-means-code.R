# check working directory
getwd()

# import .csv file and read it
kademo1 <- read.csv("KaDemo.csv", TRUE, ",")
kademo1

#processing data, try to see if there are missing values
#if there are some missing values, try to see if na.omit() is useful
is.na(kademo1)

# before performing k-means clusters with kademo data 
# remove the PrefereceGroup variable by setting it to NULL
kademo = kademo1
kademo$PreferenceGroup = NULL

#view the first 6 rows of the data
head(kademo, n = 6)

#understand basic infomation about data, to see if it needs to be scale
#because the sd/varience here is not large, it is unnecessary to scale the data
stats_kademo = data.frame(Min = apply(kademo, 2, min), 
                        Med = apply(kademo, 2, median), 
                        Mean = apply(kademo, 2, mean), 
                        SD = apply(kademo, 2, sd), 
                        Max = apply(kademo, 2, max))
stats_kademo = round(stats_kademo,1)
stats_kademo

#calculate cluster tendency to see if the data is clusterable
#if the value < 0.5, it means the data is clusterable
library(factoextra)
tendency_kademo = get_clust_tendency(kademo,200, graph = TRUE)
tendency_kademo$hopkins_stat
tendency_kademo$plot

#try to calculate the sum of squared error and define the number of k
library(ggplot2)
set.seed(123)
fviz_nbclust(kademo, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

#then we know 4 is the best number of k
#visualize clusters
kmeans_kademo =  kmeans(kademo, 4)
kmeans_kademo
fviz_cluster(kmeans_kademo, data = kademo)

# compare the Preference label with the cluster result
table(kademo1$PreferenceGroup, kmeans_kademo$cluster)                 

----------------------------------------------------------------------------------------------------------------------
#The results are as follows:

> # check working directory
> getwd()
[1] "/Users/kathrynkang/Documents/Rstudioclass/Ford_Ka"
> 
> # import .csv file and read it
> kademo1 <- read.csv("KaDemo.csv", TRUE, ",")
> kademo1
    PreferenceGroup Gender MaritalStatus FirstTimePurchase AgeCategory ChildrenCategory
1                 1      2             3                 2           5                0
2                 3      1             2                 1           1                1
3                 2      2             3                 2           3                1
4                 3      1             3                 2           5                0
5                 1      2             1                 1           5                2
6                 1      2             1                 1           2                1
7                 1      1             3                 2           3                0
8                 3      1             3                 2           6                0
9                 1      2             1                 2           3                2
10                1      2             3                 2           3                0
11                3      1             2                 2           6                0
12                1      2             3                 2           5                0
13                1      1             1                 2           5                0
14                1      1             1                 2           1                0
15                2      1             1                 2           4                1
16                2      1             1                 2           6                0
17                1      2             1                 2           2                1
18                3      2             1                 2           3                0
19                3      2             1                 2           4                0
20                3      2             1                 2           1                0
21                2      1             1                 2           5                1
22                1      2             1                 2           5                2
23                1      2             1                 2           6                0
24                1      2             1                 2           3                2
25                2      2             3                 2           6                0
26                1      1             3                 2           6                0
27                1      2             3                 2           5                0
28                1      2             2                 2           5                2
29                1      2             3                 1           3                1
30                1      1             1                 2           5                2
31                1      2             1                 2           5                1
32                1      1             3                 2           6                0
33                3      1             3                 2           4                0
34                2      1             3                 2           3                0
35                2      2             1                 2           3                0
36                3      2             1                 1           2                0
37                2      2             3                 2           6                2
38                2      1             3                 2           3                2
39                3      2             1                 2           2                0
40                2      1             1                 2           2                0
41                1      1             3                 2           5                0
42                1      1             3                 2           2                2
43                2      1             1                 2           2                0
44                1      1             2                 2           1                1
45                1      2             3                 2           6                0
46                1      2             3                 2           5                2
47                1      1             1                 2           1                0
48                3      1             1                 2           5                1
49                2      2             1                 2           5                1
50                1      2             3                 2           3                0
51                3      1             2                 2           2                0
52                1      2             3                 2           5                1
53                1      1             2                 2           5                0
54                2      1             1                 2           3                0
55                1      1             1                 2           6                0
56                1      1             1                 2           2                1
57                3      1             1                 2           3                0
58                2      2             2                 2           5                0
59                2      1             2                 2           5                0
60                1      2             1                 2           1                0
61                3      1             1                 2           3                2
62                2      1             2                 2           4                0
63                3      1             3                 2           5                0
64                1      1             3                 2           6                0
65                1      1             1                 2           5                1
66                2      1             3                 2           5                2
67                3      2             1                 1           5                2
68                3      1             3                 2           6                0
69                1      1             1                 2           3                1
70                1      2             1                 2           3                2
71                1      2             1                 2           1                0
72                2      1             3                 1           4                2
73                1      2             3                 2           5                1
74                1      2             1                 2           5                0
75                1      2             1                 2           3                1
76                1      1             3                 2           2                1
77                1      1             3                 2           6                2
78                1      1             1                 2           4                2
79                1      2             1                 2           1                2
80                1      2             1                 1           3                1
81                1      2             1                 2           5                1
82                2      2             3                 2           3                0
83                1      2             1                 2           4                0
84                3      1             1                 2           3                2
85                1      2             3                 2           6                0
86                1      1             1                 2           6                0
87                3      2             1                 1           3                0
88                1      1             1                 2           3                2
89                2      1             3                 2           2                1
90                3      1             2                 2           3                0
91                1      2             1                 1           3                2
92                1      2             1                 1           2                1
93                2      2             1                 2           5                2
94                3      1             3                 2           3                0
95                2      2             1                 2           6                2
96                3      1             3                 1           2                0
97                3      2             3                 1           2                0
98                2      2             1                 2           6                2
99                3      2             3                 2           4                2
100               1      1             1                 2           5                2
101               2      2             1                 1           5                0
102               1      2             1                 2           2                0
103               2      2             1                 2           5                0
104               1      2             3                 2           4                0
105               2      2             1                 2           3                2
106               1      2             3                 2           2                0
107               1      1             3                 2           5                0
108               1      2             1                 2           6                2
109               3      1             3                 2           3                0
110               1      2             2                 2           2                2
111               1      2             3                 2           4                0
112               3      2             3                 2           5                1
113               1      1             3                 2           5                2
114               3      1             3                 2           5                0
115               3      1             2                 2           6                1
116               2      1             3                 2           2                0
117               2      2             1                 2           2                0
118               2      1             1                 2           6                0
119               2      2             1                 1           4                0
120               3      2             3                 2           2                2
121               2      2             1                 2           5                0
122               2      1             3                 2           6                1
123               1      1             1                 2           6                0
124               1      1             3                 2           2                2
125               1      1             3                 1           1                0
126               3      1             3                 1           1                0
127               3      2             3                 2           3                0
128               2      2             3                 2           4                1
129               1      2             3                 2           3                2
130               2      1             3                 2           5                0
131               2      2             1                 2           3                2
132               3      2             1                 2           4                2
133               3      2             2                 1           5                0
134               3      1             1                 2           2                0
135               1      2             1                 1           5                1
136               1      2             1                 2           2                0
137               3      2             3                 1           1                0
138               3      2             1                 2           2                0
139               2      2             3                 2           6                2
140               2      1             1                 2           5                0
141               2      2             1                 2           4                2
142               2      1             3                 2           2                0
    IncomeCategory
1                6
2                3
3                1
4                3
5                4
6                4
7                6
8                4
9                1
10               4
11               2
12               6
13               2
14               5
15               2
16               6
17               3
18               2
19               4
20               5
21               4
22               5
23               5
24               2
25               2
26               1
27               4
28               3
29               6
30               5
31               2
32               6
33               6
34               2
35               2
36               1
37               6
38               3
39               4
40               5
41               5
42               5
43               4
44               3
45               4
46               2
47               3
48               3
49               5
50               6
51               5
52               2
53               1
54               4
55               5
56               2
57               4
58               3
59               3
60               5
61               5
62               4
63               4
64               6
65               5
66               3
67               3
68               4
69               5
70               3
71               1
72               3
73               6
74               3
75               5
76               5
77               6
78               5
79               6
80               1
81               2
82               3
83               5
84               5
85               2
86               6
87               5
88               6
89               3
90               2
91               4
92               2
93               4
94               2
95               4
96               5
97               6
98               2
99               3
100              6
101              3
102              5
103              1
104              3
105              6
106              1
107              4
108              3
109              2
110              3
111              3
112              4
113              5
114              4
115              2
116              1
117              3
118              5
119              2
120              5
121              2
122              4
123              5
124              5
125              1
126              1
127              2
128              5
129              6
130              2
131              5
132              4
133              5
134              1
135              6
136              6
137              1
138              3
139              3
140              3
141              4
142              2
 [ reached 'max' / getOption("max.print") -- omitted 108 rows ]
> 
> #processing data, try to see if there are missing values
> #if there are some missing values, try to see if na.omit() is useful
> is.na(kademo1)
       PreferenceGroup Gender MaritalStatus FirstTimePurchase AgeCategory ChildrenCategory
  [1,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [2,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [3,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [4,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [5,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [6,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [7,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [8,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
  [9,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [10,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [11,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [12,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [13,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [14,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [15,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [16,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [17,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [18,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [19,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [20,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [21,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [22,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [23,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [24,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [25,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [26,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [27,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [28,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [29,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [30,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [31,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [32,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [33,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [34,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [35,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [36,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [37,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [38,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [39,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [40,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [41,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [42,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [43,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [44,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [45,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [46,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [47,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [48,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [49,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [50,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [51,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [52,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [53,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [54,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [55,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [56,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [57,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [58,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [59,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [60,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [61,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [62,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [63,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [64,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [65,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [66,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [67,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [68,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [69,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [70,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [71,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [72,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [73,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [74,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [75,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [76,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [77,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [78,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [79,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [80,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [81,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [82,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [83,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [84,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [85,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [86,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [87,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [88,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [89,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [90,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [91,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [92,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [93,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [94,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [95,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [96,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [97,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [98,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
 [99,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[100,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[101,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[102,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[103,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[104,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[105,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[106,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[107,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[108,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[109,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[110,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[111,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[112,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[113,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[114,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[115,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[116,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[117,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[118,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[119,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[120,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[121,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[122,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[123,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[124,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[125,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[126,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[127,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[128,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[129,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[130,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[131,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[132,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[133,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[134,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[135,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[136,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[137,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[138,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[139,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[140,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[141,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
[142,]           FALSE  FALSE         FALSE             FALSE       FALSE            FALSE
       IncomeCategory
  [1,]          FALSE
  [2,]          FALSE
  [3,]          FALSE
  [4,]          FALSE
  [5,]          FALSE
  [6,]          FALSE
  [7,]          FALSE
  [8,]          FALSE
  [9,]          FALSE
 [10,]          FALSE
 [11,]          FALSE
 [12,]          FALSE
 [13,]          FALSE
 [14,]          FALSE
 [15,]          FALSE
 [16,]          FALSE
 [17,]          FALSE
 [18,]          FALSE
 [19,]          FALSE
 [20,]          FALSE
 [21,]          FALSE
 [22,]          FALSE
 [23,]          FALSE
 [24,]          FALSE
 [25,]          FALSE
 [26,]          FALSE
 [27,]          FALSE
 [28,]          FALSE
 [29,]          FALSE
 [30,]          FALSE
 [31,]          FALSE
 [32,]          FALSE
 [33,]          FALSE
 [34,]          FALSE
 [35,]          FALSE
 [36,]          FALSE
 [37,]          FALSE
 [38,]          FALSE
 [39,]          FALSE
 [40,]          FALSE
 [41,]          FALSE
 [42,]          FALSE
 [43,]          FALSE
 [44,]          FALSE
 [45,]          FALSE
 [46,]          FALSE
 [47,]          FALSE
 [48,]          FALSE
 [49,]          FALSE
 [50,]          FALSE
 [51,]          FALSE
 [52,]          FALSE
 [53,]          FALSE
 [54,]          FALSE
 [55,]          FALSE
 [56,]          FALSE
 [57,]          FALSE
 [58,]          FALSE
 [59,]          FALSE
 [60,]          FALSE
 [61,]          FALSE
 [62,]          FALSE
 [63,]          FALSE
 [64,]          FALSE
 [65,]          FALSE
 [66,]          FALSE
 [67,]          FALSE
 [68,]          FALSE
 [69,]          FALSE
 [70,]          FALSE
 [71,]          FALSE
 [72,]          FALSE
 [73,]          FALSE
 [74,]          FALSE
 [75,]          FALSE
 [76,]          FALSE
 [77,]          FALSE
 [78,]          FALSE
 [79,]          FALSE
 [80,]          FALSE
 [81,]          FALSE
 [82,]          FALSE
 [83,]          FALSE
 [84,]          FALSE
 [85,]          FALSE
 [86,]          FALSE
 [87,]          FALSE
 [88,]          FALSE
 [89,]          FALSE
 [90,]          FALSE
 [91,]          FALSE
 [92,]          FALSE
 [93,]          FALSE
 [94,]          FALSE
 [95,]          FALSE
 [96,]          FALSE
 [97,]          FALSE
 [98,]          FALSE
 [99,]          FALSE
[100,]          FALSE
[101,]          FALSE
[102,]          FALSE
[103,]          FALSE
[104,]          FALSE
[105,]          FALSE
[106,]          FALSE
[107,]          FALSE
[108,]          FALSE
[109,]          FALSE
[110,]          FALSE
[111,]          FALSE
[112,]          FALSE
[113,]          FALSE
[114,]          FALSE
[115,]          FALSE
[116,]          FALSE
[117,]          FALSE
[118,]          FALSE
[119,]          FALSE
[120,]          FALSE
[121,]          FALSE
[122,]          FALSE
[123,]          FALSE
[124,]          FALSE
[125,]          FALSE
[126,]          FALSE
[127,]          FALSE
[128,]          FALSE
[129,]          FALSE
[130,]          FALSE
[131,]          FALSE
[132,]          FALSE
[133,]          FALSE
[134,]          FALSE
[135,]          FALSE
[136,]          FALSE
[137,]          FALSE
[138,]          FALSE
[139,]          FALSE
[140,]          FALSE
[141,]          FALSE
[142,]          FALSE
 [ reached getOption("max.print") -- omitted 108 rows ]
> 
> # before performing k-means clusters with kademo data 
> # remove the PrefereceGroup variable by setting it to NULL
> kademo = kademo1
> kademo$PreferenceGroup = NULL
> 
> #view the first 6 rows of the data
> head(kademo, n=6)
  Gender MaritalStatus FirstTimePurchase AgeCategory ChildrenCategory IncomeCategory
1      2             3                 2           5                0              6
2      1             2                 1           1                1              3
3      2             3                 2           3                1              1
4      1             3                 2           5                0              3
5      2             1                 1           5                2              4
6      2             1                 1           2                1              4
> 
> #understand basic infomation about data, to see if it needs to be scale
> #because the sd/varience here is not large, it is unnecessary to scale the data
> stats_kademo = data.frame(Min = apply(kademo, 2, min), 
+                         Med = apply(kademo, 2, median), 
+                         Mean = apply(kademo, 2, mean), 
+                         SD = apply(kademo, 2, sd), 
+                         Max = apply(kademo, 2, max))
> stats_kademo = round(stats_kademo,1)
> stats_kademo
                  Min Med Mean  SD Max
Gender              1   1  1.5 0.5   2
MaritalStatus       1   1  1.9 0.9   3
FirstTimePurchase   1   2  1.9 0.4   2
AgeCategory         1   4  3.8 1.6   6
ChildrenCategory    0   0  0.6 0.8   2
IncomeCategory      1   4  3.7 1.6   6
> 
> #calculate cluster tendency to see if the data is clusterable
> #if the value < 0.5, it means the data is clusterable
> library(factoextra)
> tendency_kademo = get_clust_tendency(kademo,200, graph = TRUE)
> tendency_kademo$hopkins_stat
[1] 0.4881671
> tendency_kademo$plot
> 
> #try to calculate the sum of squared error and define the number of k
> library(ggplot2)
> set.seed(123)
> fviz_nbclust(kademo, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
> 
> #then we know 4 is the best number of k
> #visualize clusters
> kmeans_kademo =  kmeans(kademo, 4)
> kmeans_kademo
K-means clustering with 4 clusters of sizes 85, 57, 48, 60

Cluster means:
    Gender MaritalStatus FirstTimePurchase AgeCategory ChildrenCategory IncomeCategory
1 1.552941      1.976471          1.882353    5.082353        0.6941176       2.976471
2 1.491228      1.754386          1.824561    2.315789        0.7368421       4.982456
3 1.354167      1.791667          1.937500    5.145833        0.5625000       5.479167
4 1.466667      1.900000          1.766667    2.183333        0.4666667       2.000000

Clustering vector:
  [1] 3 4 4 1 1 2 2 1 4 2 1 3 1 2 1 3 4 4 1 2 1 3 3 4 1 1 1 1 2 3 1 3 3 4 4 4 3 4 2 2 3 2 2
 [44] 4 1 1 4 1 3 2 2 1 1 2 3 4 2 1 1 2 2 1 1 3 3 1 1 1 2 4 4 1 3 1 2 2 3 3 2 4 1 4 3 2 1 3
 [87] 2 2 4 4 2 4 1 4 1 2 2 1 1 3 1 2 1 1 2 4 1 1 4 4 1 1 3 1 1 4 4 3 1 2 1 1 3 2 4 4 4 3 2
[130] 1 2 1 3 4 3 2 4 4 1 1 1 4 1 3 1 1 3 1 1 4 1 4 3 2 4 4 2 3 1 1 1 1 4 2 4 1 1 1 2 1 4 1
[173] 4 1 4 2 2 2 1 2 1 4 3 2 3 3 4 4 2 3 1 4 4 3 2 1 4 2 4 2 3 3 2 2 3 2 1 1 1 1 4 4 1 3 1
[216] 4 3 1 2 3 1 3 3 2 3 1 3 2 3 2 4 1 2 1 4 4 4 1 4 2 4 1 4 4 3 3 1 2 2 1

Within cluster sum of squares by cluster:
[1] 274.2118 169.4035 127.4792 194.9833
 (between_SS / total_SS =  56.1 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
[6] "betweenss"    "size"         "iter"         "ifault"      
> fviz_cluster(kmeans_kademo, data = kademo)

> # compare the Preference label with the cluster result
> table(kademo1$PreferenceGroup, kmeans_kademo$cluster)
   
     1  2  3  4
  1 35 30 28 23
  2 30 12 13 17
  3 20 15  7 20
