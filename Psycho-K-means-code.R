# check working directory
getwd()

# import .csv file and read it
kapsy <- read.csv("Kapsycho.csv", TRUE, ",")
kapsy

#processing data, try to see if there are missing values
#if there are some missing values, try to see if na.omit() is useful
is.na(kapsy)

#understand basic infomation about data, to see if it needs to be scale
#because the sd/varience here is not large, it is unnecessary to scale the data
stats_kapsy = data.frame(Min = apply(kapsy, 2, min), 
                          Med = apply(kapsy, 2, median), 
                          Mean = apply(kapsy, 2, mean), 
                          SD = apply(kapsy, 2, sd), 
                          Max = apply(kapsy, 2, max))
stats_kapsy = round(stats_kapsy,1)
stats_kapsy

#calculate cluster tendency to see if the data is clusterable
#if the value < 0.5, it means the data is clusterable
library(factoextra)
library(ggplot2)
tendency_kapsy = get_clust_tendency(kapsy,200, graph = TRUE)
tendency_kapsy$hopkins_stat
tendency_kapsy$plot

#try to calculate the sum of squared error and define the number of k
set.seed(123)
fviz_nbclust(kapsy, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

#then we know 4 is the best number of k
#visualize clusters
kmeans_kapsy =  kmeans(kapsy, 4)
kmeans_kapsy
fviz_cluster(kmeans_kapsy, data = kapsy)
-------------------------------------------------------------------------------------------------------
Results are as follows:

> # check working directory
> getwd()
[1] "/Users/kathrynkang/Documents/Rstudioclass/Ford_Ka"
> 
> # import .csv file and read it
> kapsy <- read.csv("Kapsycho.csv", TRUE, ",")
> kapsy
    Fashion.Trendy Reliabililty.Dependability Low.fuel.consumption Performance
1             4.75                   3.666667                  6.0    4.142857
2             7.00                   3.666667                  4.0    3.857143
3             4.75                   3.666667                  4.5    4.142857
4             4.75                   4.000000                  5.5    3.857143
5             5.00                   4.000000                  3.5    4.857143
6             6.25                   3.000000                  4.0    3.000000
7             6.50                   3.000000                  4.5    2.714286
8             6.50                   2.666667                  4.0    3.285714
9             5.75                   4.666667                  4.0    5.571429
10            4.75                   3.666667                  5.5    4.142857
11            6.50                   4.666667                  5.0    3.000000
12            5.75                   3.333333                  3.0    4.428571
13            5.50                   5.000000                  4.5    5.142857
14            3.75                   4.000000                  4.0    4.285714
15            5.75                   2.333333                  4.5    3.428571
16            3.75                   3.666667                  5.0    3.428571
17            6.00                   3.000000                  3.5    3.285714
18            4.25                   4.000000                  5.5    4.714286
19            6.25                   2.666667                  3.5    3.714286
20            5.25                   5.000000                  4.0    5.428571
21            4.00                   4.666667                  3.5    4.714286
22            5.25                   3.666667                  3.0    4.285714
23            6.75                   3.000000                  4.5    3.428571
24            5.00                   3.333333                  5.0    4.000000
25            4.75                   3.000000                  4.5    4.428571
26            5.75                   4.666667                  3.5    5.142857
27            5.25                   2.666667                  3.5    4.428571
28            3.25                   4.666667                  5.5    4.142857
29            3.75                   4.000000                  5.0    3.857143
30            4.50                   4.333333                  4.5    4.285714
31            4.25                   4.333333                  3.5    4.857143
32            6.00                   4.000000                  4.5    3.571429
33            5.00                   4.666667                  4.5    4.285714
34            4.50                   3.666667                  5.5    3.857143
35            5.50                   4.333333                  4.5    4.142857
36            5.50                   4.000000                  4.5    4.000000
37            4.25                   4.000000                  3.5    4.285714
38            5.00                   4.333333                  5.5    4.000000
39            6.25                   2.000000                  3.5    3.285714
40            4.50                   3.666667                  4.5    4.571429
41            6.50                   3.333333                  4.0    3.000000
42            4.75                   4.333333                  5.5    4.571429
43            4.25                   3.666667                  4.5    4.000000
44            3.75                   4.333333                  5.0    4.857143
45            5.75                   3.000000                  3.5    4.285714
46            5.00                   3.333333                  2.5    3.571429
47            3.25                   4.000000                  5.0    4.714286
48            3.50                   4.000000                  5.0    4.142857
49            5.00                   4.333333                  5.5    4.000000
50            4.75                   3.666667                  4.0    4.428571
51            3.75                   4.000000                  5.5    4.857143
52            4.75                   5.666667                  3.5    5.000000
53            4.50                   5.666667                  4.5    4.000000
54            4.75                   4.333333                  4.0    4.571429
55            6.00                   4.666667                  4.0    5.428571
56            6.75                   4.000000                  2.5    5.000000
57            4.75                   4.000000                  5.5    4.714286
58            5.25                   3.666667                  5.5    3.857143
59            6.25                   2.333333                  5.0    3.142857
60            5.00                   3.666667                  3.5    4.285714
61            3.75                   4.000000                  5.0    4.000000
62            4.00                   4.666667                  5.0    4.428571
63            5.75                   3.333333                  5.0    3.142857
64            4.00                   5.000000                  5.5    4.142857
65            5.25                   4.000000                  3.5    4.571429
66            4.50                   4.333333                  5.5    4.571429
67            5.75                   3.666667                  2.5    2.428571
68            6.00                   2.333333                  3.5    3.428571
69            4.75                   5.333333                  6.0    4.000000
70            5.75                   3.000000                  3.5    3.000000
71            6.25                   4.333333                  4.0    2.857143
72            3.25                   3.333333                  3.5    4.428571
73            5.50                   5.666667                  2.5    5.285714
74            5.25                   4.333333                  5.0    4.714286
75            6.75                   4.000000                  5.5    3.285714
76            3.75                   4.333333                  5.5    4.571429
77            4.50                   4.333333                  4.0    4.142857
78            4.00                   3.000000                  4.0    4.857143
79            4.50                   4.333333                  5.5    4.142857
80            3.25                   4.666667                  4.5    4.428571
81            7.25                   3.000000                  4.0    3.571429
82            4.75                   4.000000                  4.5    4.000000
83            6.00                   2.666667                  3.5    3.142857
84            6.50                   3.333333                  5.5    3.000000
85            4.00                   3.000000                  3.0    4.714286
86            6.25                   3.666667                  2.5    3.285714
87            4.50                   4.333333                  5.0    4.428571
88            5.50                   3.666667                  3.5    3.428571
89            5.25                   5.000000                  3.0    5.142857
90            5.00                   4.333333                  3.0    4.714286
91            3.75                   4.000000                  6.0    4.285714
92            6.25                   5.333333                  5.5    4.714286
93            4.75                   4.333333                  4.5    4.285714
94            3.75                   3.333333                  5.0    4.428571
95            5.75                   3.666667                  4.0    4.571429
96            6.75                   3.333333                  4.5    3.000000
97            3.75                   4.333333                  5.5    4.142857
98            3.75                   3.666667                  4.5    4.142857
99            6.75                   2.666667                  4.5    2.714286
100           5.25                   4.666667                  4.5    5.428571
    Prefer.domestic.brand Basic.needs Prefer.small.car For.woman.kids Value.money
1                    3.00        3.25                3            2.0           6
2                    6.00        5.00                3            3.5           3
3                    3.75        4.50                2            4.5           3
4                    3.50        2.75                2            2.0           6
5                    4.00        3.75                1            2.0           4
6                    4.25        4.25                3            4.0           2
7                    5.00        4.50                2            3.5           3
8                    5.25        4.25                4            4.5           4
9                    3.00        5.00                5            3.0           7
10                   3.50        3.00                4            3.0           7
11                   5.25        4.50                3            3.5           3
12                   4.00        4.75                3            3.5           5
13                   2.75        4.75                3            3.0           7
14                   4.00        3.50                3            1.5           6
15                   4.75        3.75                4            5.0           4
16                   3.00        3.00                5            2.5           7
17                   5.50        3.50                2            3.5           4
18                   3.75        2.50                3            2.0           6
19                   4.25        3.75                4            4.0           6
20                   3.25        4.75                3            3.0           6
21                   5.00        4.50                2            5.0           4
22                   5.25        5.00                2            5.0           5
23                   5.50        5.00                4            5.0           3
24                   4.50        5.25                1            4.5           4
25                   3.75        3.75                1            3.5           5
26                   2.50        4.75                4            3.0           7
27                   5.00        4.75                3            3.0           6
28                   3.25        2.25                3            2.0           5
29                   3.50        3.25                4            2.0           7
30                   3.00        3.00                5            2.0           6
31                   4.75        4.25                2            2.5           4
32                   5.50        4.50                4            3.5           5
33                   2.50        3.25                3            2.0           6
34                   4.00        4.00                3            2.0           5
35                   4.50        4.75                2            3.0           4
36                   3.75        4.25                3            3.5           4
37                   4.50        4.25                2            4.0           4
38                   4.25        4.50                2            3.0           4
39                   4.75        4.25                3            3.5           4
40                   4.00        3.25                3            2.5           6
41                   5.00        4.00                4            4.0           2
42                   3.25        3.00                5            2.5           7
43                   4.50        4.25                1            4.0           2
44                   3.75        3.50                2            1.5           7
45                   4.75        5.00                2            3.5           5
46                   4.50        4.75                3            2.0           6
47                   4.00        2.75                4            2.0           7
48                   3.25        3.25                6            3.0           6
49                   4.50        4.00                3            2.0           3
50                   4.00        3.00                5            3.0           6
51                   3.75        2.75                4            2.0           6
52                   2.75        4.25                3            2.5           7
53                   5.25        5.25                3            4.0           4
54                   4.75        4.50                1            4.5           6
55                   3.00        4.50                4            3.0           6
56                   2.25        4.75                3            4.0           6
57                   3.75        3.25                4            2.0           6
58                   5.00        4.75                2            4.5           4
59                   5.50        4.75                4            4.0           5
60                   4.00        4.00                1            5.0           2
61                   3.75        2.75                4            1.5           7
62                   4.00        3.50                2            2.5           5
63                   5.25        3.75                4            4.0           3
64                   3.75        2.75                2            1.5           6
65                   3.00        4.50                5            3.0           6
66                   4.50        4.25                3            4.5           4
67                   5.25        4.00                3            3.0           4
68                   4.00        3.75                5            3.0           5
69                   5.00        4.25                4            2.0           5
70                   4.50        3.25                4            4.5           4
71                   5.50        4.00                4            3.5           5
72                   3.00        2.75                3            2.5           6
73                   2.00        4.50                4            1.5           6
74                   4.50        5.00                3            3.5           5
75                   4.50        4.00                4            4.5           4
76                   3.50        3.75                3            2.5           5
77                   3.50        3.00                2            2.0           6
78                   4.50        3.25                2            4.0           4
79                   3.25        2.75                4            1.5           6
80                   4.50        3.00                5            3.0           6
81                   4.25        4.75                3            2.5           5
82                   4.75        4.25                2            4.5           4
83                   4.75        4.25                4            4.0           4
84                   5.50        4.75                5            4.5           3
85                   4.75        4.25                1            4.0           3
86                   4.50        3.75                4            3.5           3
87                   4.00        3.75                4            1.5           5
88                   4.25        3.50                5            4.5           5
89                   2.50        4.25                2            3.0           6
90                   5.00        4.75                3            4.0           5
91                   3.75        3.00                4            1.5           7
92                   2.75        5.25                4            1.5           7
93                   4.25        4.50                2            4.5           4
94                   2.75        2.25                5            1.0           6
95                   4.25        4.75                1            3.0           4
96                   4.75        4.25                5            4.0           4
97                   3.50        3.00                5            1.5           6
98                   3.50        3.25                4            1.5           6
99                   3.75        3.75                4            5.0           5
100                  2.25        4.75                4            3.0           7
    Custom.design
1             5.0
2             5.5
3             4.0
4             3.5
5             3.0
6             6.0
7             5.0
8             6.0
9             5.5
10            3.5
11            5.5
12            5.0
13            5.0
14            3.0
15            5.0
16            4.0
17            5.5
18            4.0
19            5.5
20            5.5
21            4.0
22            2.0
23            5.5
24            3.0
25            4.5
26            5.0
27            3.5
28            4.5
29            2.5
30            3.0
31            6.0
32            5.0
33            4.0
34            4.5
35            3.0
36            3.5
37            4.0
38            5.0
39            5.5
40            3.0
41            5.5
42            4.0
43            3.5
44            3.0
45            2.5
46            5.0
47            3.0
48            4.5
49            3.0
50            5.5
51            4.0
52            5.0
53            3.5
54            2.0
55            5.5
56            6.0
57            4.0
58            4.0
59            4.5
60            4.5
61            3.5
62            4.0
63            5.0
64            5.5
65            5.5
66            4.0
67            5.5
68            4.5
69            4.0
70            5.5
71            5.0
72            4.5
73            4.5
74            4.5
75            5.0
76            4.0
77            4.5
78            5.0
79            4.0
80            3.5
81            6.0
82            3.0
83            5.5
84            5.0
85            4.0
86            5.0
87            4.0
88            5.5
89            5.0
90            3.5
91            5.0
92            4.5
93            3.0
94            3.5
95            4.0
96            4.0
97            3.0
98            4.0
99            5.5
100           5.5
 [ reached 'max' / getOption("max.print") -- omitted 150 rows ]
> 
> #processing data, try to see if there are missing values
> #if there are some missing values, try to see if na.omit() is useful
> is.na(kapsy)
       Fashion.Trendy Reliabililty.Dependability Low.fuel.consumption Performance
  [1,]          FALSE                      FALSE                FALSE       FALSE
  [2,]          FALSE                      FALSE                FALSE       FALSE
  [3,]          FALSE                      FALSE                FALSE       FALSE
  [4,]          FALSE                      FALSE                FALSE       FALSE
  [5,]          FALSE                      FALSE                FALSE       FALSE
  [6,]          FALSE                      FALSE                FALSE       FALSE
  [7,]          FALSE                      FALSE                FALSE       FALSE
  [8,]          FALSE                      FALSE                FALSE       FALSE
  [9,]          FALSE                      FALSE                FALSE       FALSE
 [10,]          FALSE                      FALSE                FALSE       FALSE
 [11,]          FALSE                      FALSE                FALSE       FALSE
 [12,]          FALSE                      FALSE                FALSE       FALSE
 [13,]          FALSE                      FALSE                FALSE       FALSE
 [14,]          FALSE                      FALSE                FALSE       FALSE
 [15,]          FALSE                      FALSE                FALSE       FALSE
 [16,]          FALSE                      FALSE                FALSE       FALSE
 [17,]          FALSE                      FALSE                FALSE       FALSE
 [18,]          FALSE                      FALSE                FALSE       FALSE
 [19,]          FALSE                      FALSE                FALSE       FALSE
 [20,]          FALSE                      FALSE                FALSE       FALSE
 [21,]          FALSE                      FALSE                FALSE       FALSE
 [22,]          FALSE                      FALSE                FALSE       FALSE
 [23,]          FALSE                      FALSE                FALSE       FALSE
 [24,]          FALSE                      FALSE                FALSE       FALSE
 [25,]          FALSE                      FALSE                FALSE       FALSE
 [26,]          FALSE                      FALSE                FALSE       FALSE
 [27,]          FALSE                      FALSE                FALSE       FALSE
 [28,]          FALSE                      FALSE                FALSE       FALSE
 [29,]          FALSE                      FALSE                FALSE       FALSE
 [30,]          FALSE                      FALSE                FALSE       FALSE
 [31,]          FALSE                      FALSE                FALSE       FALSE
 [32,]          FALSE                      FALSE                FALSE       FALSE
 [33,]          FALSE                      FALSE                FALSE       FALSE
 [34,]          FALSE                      FALSE                FALSE       FALSE
 [35,]          FALSE                      FALSE                FALSE       FALSE
 [36,]          FALSE                      FALSE                FALSE       FALSE
 [37,]          FALSE                      FALSE                FALSE       FALSE
 [38,]          FALSE                      FALSE                FALSE       FALSE
 [39,]          FALSE                      FALSE                FALSE       FALSE
 [40,]          FALSE                      FALSE                FALSE       FALSE
 [41,]          FALSE                      FALSE                FALSE       FALSE
 [42,]          FALSE                      FALSE                FALSE       FALSE
 [43,]          FALSE                      FALSE                FALSE       FALSE
 [44,]          FALSE                      FALSE                FALSE       FALSE
 [45,]          FALSE                      FALSE                FALSE       FALSE
 [46,]          FALSE                      FALSE                FALSE       FALSE
 [47,]          FALSE                      FALSE                FALSE       FALSE
 [48,]          FALSE                      FALSE                FALSE       FALSE
 [49,]          FALSE                      FALSE                FALSE       FALSE
 [50,]          FALSE                      FALSE                FALSE       FALSE
 [51,]          FALSE                      FALSE                FALSE       FALSE
 [52,]          FALSE                      FALSE                FALSE       FALSE
 [53,]          FALSE                      FALSE                FALSE       FALSE
 [54,]          FALSE                      FALSE                FALSE       FALSE
 [55,]          FALSE                      FALSE                FALSE       FALSE
 [56,]          FALSE                      FALSE                FALSE       FALSE
 [57,]          FALSE                      FALSE                FALSE       FALSE
 [58,]          FALSE                      FALSE                FALSE       FALSE
 [59,]          FALSE                      FALSE                FALSE       FALSE
 [60,]          FALSE                      FALSE                FALSE       FALSE
 [61,]          FALSE                      FALSE                FALSE       FALSE
 [62,]          FALSE                      FALSE                FALSE       FALSE
 [63,]          FALSE                      FALSE                FALSE       FALSE
 [64,]          FALSE                      FALSE                FALSE       FALSE
 [65,]          FALSE                      FALSE                FALSE       FALSE
 [66,]          FALSE                      FALSE                FALSE       FALSE
 [67,]          FALSE                      FALSE                FALSE       FALSE
 [68,]          FALSE                      FALSE                FALSE       FALSE
 [69,]          FALSE                      FALSE                FALSE       FALSE
 [70,]          FALSE                      FALSE                FALSE       FALSE
 [71,]          FALSE                      FALSE                FALSE       FALSE
 [72,]          FALSE                      FALSE                FALSE       FALSE
 [73,]          FALSE                      FALSE                FALSE       FALSE
 [74,]          FALSE                      FALSE                FALSE       FALSE
 [75,]          FALSE                      FALSE                FALSE       FALSE
 [76,]          FALSE                      FALSE                FALSE       FALSE
 [77,]          FALSE                      FALSE                FALSE       FALSE
 [78,]          FALSE                      FALSE                FALSE       FALSE
 [79,]          FALSE                      FALSE                FALSE       FALSE
 [80,]          FALSE                      FALSE                FALSE       FALSE
 [81,]          FALSE                      FALSE                FALSE       FALSE
 [82,]          FALSE                      FALSE                FALSE       FALSE
 [83,]          FALSE                      FALSE                FALSE       FALSE
 [84,]          FALSE                      FALSE                FALSE       FALSE
 [85,]          FALSE                      FALSE                FALSE       FALSE
 [86,]          FALSE                      FALSE                FALSE       FALSE
 [87,]          FALSE                      FALSE                FALSE       FALSE
 [88,]          FALSE                      FALSE                FALSE       FALSE
 [89,]          FALSE                      FALSE                FALSE       FALSE
 [90,]          FALSE                      FALSE                FALSE       FALSE
 [91,]          FALSE                      FALSE                FALSE       FALSE
 [92,]          FALSE                      FALSE                FALSE       FALSE
 [93,]          FALSE                      FALSE                FALSE       FALSE
 [94,]          FALSE                      FALSE                FALSE       FALSE
 [95,]          FALSE                      FALSE                FALSE       FALSE
 [96,]          FALSE                      FALSE                FALSE       FALSE
 [97,]          FALSE                      FALSE                FALSE       FALSE
 [98,]          FALSE                      FALSE                FALSE       FALSE
 [99,]          FALSE                      FALSE                FALSE       FALSE
[100,]          FALSE                      FALSE                FALSE       FALSE
       Prefer.domestic.brand Basic.needs Prefer.small.car For.woman.kids Value.money
  [1,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [2,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [3,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [4,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [5,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [6,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [7,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [8,]                 FALSE       FALSE            FALSE          FALSE       FALSE
  [9,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [10,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [11,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [12,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [13,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [14,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [15,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [16,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [17,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [18,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [19,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [20,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [21,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [22,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [23,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [24,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [25,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [26,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [27,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [28,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [29,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [30,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [31,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [32,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [33,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [34,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [35,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [36,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [37,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [38,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [39,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [40,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [41,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [42,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [43,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [44,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [45,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [46,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [47,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [48,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [49,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [50,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [51,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [52,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [53,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [54,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [55,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [56,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [57,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [58,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [59,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [60,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [61,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [62,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [63,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [64,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [65,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [66,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [67,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [68,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [69,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [70,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [71,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [72,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [73,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [74,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [75,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [76,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [77,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [78,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [79,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [80,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [81,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [82,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [83,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [84,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [85,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [86,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [87,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [88,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [89,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [90,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [91,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [92,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [93,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [94,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [95,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [96,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [97,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [98,]                 FALSE       FALSE            FALSE          FALSE       FALSE
 [99,]                 FALSE       FALSE            FALSE          FALSE       FALSE
[100,]                 FALSE       FALSE            FALSE          FALSE       FALSE
       Custom.design
  [1,]         FALSE
  [2,]         FALSE
  [3,]         FALSE
  [4,]         FALSE
  [5,]         FALSE
  [6,]         FALSE
  [7,]         FALSE
  [8,]         FALSE
  [9,]         FALSE
 [10,]         FALSE
 [11,]         FALSE
 [12,]         FALSE
 [13,]         FALSE
 [14,]         FALSE
 [15,]         FALSE
 [16,]         FALSE
 [17,]         FALSE
 [18,]         FALSE
 [19,]         FALSE
 [20,]         FALSE
 [21,]         FALSE
 [22,]         FALSE
 [23,]         FALSE
 [24,]         FALSE
 [25,]         FALSE
 [26,]         FALSE
 [27,]         FALSE
 [28,]         FALSE
 [29,]         FALSE
 [30,]         FALSE
 [31,]         FALSE
 [32,]         FALSE
 [33,]         FALSE
 [34,]         FALSE
 [35,]         FALSE
 [36,]         FALSE
 [37,]         FALSE
 [38,]         FALSE
 [39,]         FALSE
 [40,]         FALSE
 [41,]         FALSE
 [42,]         FALSE
 [43,]         FALSE
 [44,]         FALSE
 [45,]         FALSE
 [46,]         FALSE
 [47,]         FALSE
 [48,]         FALSE
 [49,]         FALSE
 [50,]         FALSE
 [51,]         FALSE
 [52,]         FALSE
 [53,]         FALSE
 [54,]         FALSE
 [55,]         FALSE
 [56,]         FALSE
 [57,]         FALSE
 [58,]         FALSE
 [59,]         FALSE
 [60,]         FALSE
 [61,]         FALSE
 [62,]         FALSE
 [63,]         FALSE
 [64,]         FALSE
 [65,]         FALSE
 [66,]         FALSE
 [67,]         FALSE
 [68,]         FALSE
 [69,]         FALSE
 [70,]         FALSE
 [71,]         FALSE
 [72,]         FALSE
 [73,]         FALSE
 [74,]         FALSE
 [75,]         FALSE
 [76,]         FALSE
 [77,]         FALSE
 [78,]         FALSE
 [79,]         FALSE
 [80,]         FALSE
 [81,]         FALSE
 [82,]         FALSE
 [83,]         FALSE
 [84,]         FALSE
 [85,]         FALSE
 [86,]         FALSE
 [87,]         FALSE
 [88,]         FALSE
 [89,]         FALSE
 [90,]         FALSE
 [91,]         FALSE
 [92,]         FALSE
 [93,]         FALSE
 [94,]         FALSE
 [95,]         FALSE
 [96,]         FALSE
 [97,]         FALSE
 [98,]         FALSE
 [99,]         FALSE
[100,]         FALSE
 [ reached getOption("max.print") -- omitted 150 rows ]
> 
> #understand basic infomation about data, to see if it needs to be scale
> #because the sd/varience here is not large, it is unnecessary to scale the data
> stats_kapsy = data.frame(Min = apply(kapsy, 2, min), 
+                           Med = apply(kapsy, 2, median), 
+                           Mean = apply(kapsy, 2, mean), 
+                           SD = apply(kapsy, 2, sd), 
+                           Max = apply(kapsy, 2, max))
> stats_kapsy = round(stats_kapsy,1)
> stats_kapsy
                           Min Med Mean  SD Max
Fashion.Trendy             2.2 5.2  5.2 1.0 7.2
Reliabililty.Dependability 2.0 3.7  3.8 0.7 5.7
Low.fuel.consumption       1.0 4.5  4.3 0.9 6.5
Performance                2.4 4.1  4.0 0.7 5.6
Prefer.domestic.brand      2.0 4.1  4.1 0.8 6.0
Basic.needs                2.0 4.0  3.9 0.8 6.0
Prefer.small.car           1.0 3.0  3.4 1.3 7.0
For.woman.kids             1.0 3.5  3.2 1.1 6.0
Value.money                2.0 5.0  5.0 1.4 7.0
Custom.design              1.5 4.5  4.5 1.0 7.0
> 
> #calculate cluster tendency to see if the data is clusterable
> #if the value < 0.5, it means the data is clusterable
> library(factoextra)
> library(ggplot2)
> tendency_kapsy = get_clust_tendency(kapsy,200, graph = TRUE)
> tendency_kapsy$hopkins_stat
[1] 0.6695073
> tendency_kapsy$plot
> 
> #try to calculate the sum of squared error and define the number of k
> set.seed(123)
> fviz_nbclust(kapsy, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
> 
> #then we know 4 is the best number of k
> #visualize clusters
> kmeans_kapsy =  kmeans(kapsy, 4)
> kmeans_kapsy
K-means clustering with 4 clusters of sizes 34, 65, 75, 76

Cluster means:
  Fashion.Trendy Reliabililty.Dependability Low.fuel.consumption Performance
1       5.713235                   4.529412             3.852941    4.882353
2       4.938462                   3.851282             4.053846    4.193407
3       4.010000                   4.035556             5.053333    4.255238
4       6.259868                   3.210526             3.940789    3.212406
  Prefer.domestic.brand Basic.needs Prefer.small.car For.woman.kids Value.money
1              3.000000    4.713235         3.676471       2.808824    6.529412
2              4.507692    4.526923         2.015385       4.007692    4.123077
3              3.593333    2.990000         3.880000       2.013333    6.080000
4              4.677632    4.000000         3.881579       3.973684    3.907895
  Custom.design
1      5.220588
2      3.838462
3      4.086667
4      5.302632

Clustering vector:
  [1] 3 4 2 3 2 4 4 4 1 3 4 2 1 3 4 3 4 3 4 1 2 2 4 2 2 1 2 3 3 3 2 4 3 3 2 2 2 2 4 3 4 3 2
 [44] 3 2 1 3 3 2 3 3 1 2 2 1 1 3 2 4 2 3 3 4 3 1 2 4 4 3 4 4 3 1 2 4 3 3 2 3 3 4 2 4 4 2 4
 [87] 3 4 1 2 3 1 2 3 2 4 3 3 4 1 2 2 2 4 1 4 3 2 4 4 2 1 3 4 4 2 3 4 4 4 2 3 4 2 1 2 3 4 3
[130] 2 2 1 4 1 3 4 2 3 2 3 3 4 3 3 1 2 1 4 4 4 4 3 4 4 4 1 4 4 3 3 3 2 4 4 4 4 4 1 4 1 4 1
[173] 4 4 2 4 3 3 2 2 1 3 2 1 2 4 2 2 3 3 2 2 4 4 2 2 1 3 3 3 2 3 2 1 3 2 3 3 1 4 3 4 1 3 3
[216] 2 4 3 3 4 2 4 2 2 4 4 4 1 3 3 3 3 1 3 2 3 3 4 4 1 3 4 2 3 4 4 2 1 2 4

Within cluster sum of squares by cluster:
[1] 131.8593 320.1726 314.0105 335.7526
 (between_SS / total_SS =  55.6 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
[6] "betweenss"    "size"         "iter"         "ifault"      
> fviz_cluster(kmeans_kapsy, data = kapsy)
