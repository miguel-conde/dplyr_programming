# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

df <- tibble(x = 1:3, y = 3:1)

# 1. Programming Recipes ----
# 1.1. Different datasets ----

# There’s a drawback to this simple approach: it can fail silently if one of 
# the variables isn’t present in the data frame, but is present in the global 
# environment:
mutate_y <- function(df) {
  mutate(df, y = a + x)
}

df1 <- tibble(x = 1:3)
a <- 10

mutate_y(df1)

# Better:
mutate_y <- function(df) {
  mutate(df, y = .data$a + .data$x)
}

mutate_y(df1)

# 1.2 Different expressions ----
# you want one of the arguments to be a variable name (like x) or an 
# expression (like x + y)

df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)

my_summarise <- function(df, group_var) {
  df %>%
    group_by(!! group_var) %>%
    summarise(a = mean(a))
}

# Using quo()
my_summarise(df, quo(g1))

# quo() returns a quosure, which is a special type of formula.
quo(g1)

# Better with enquo() and !! inside the function:
my_summarise <- function(df, group_var) {
  group_var <- enquo(group_var)
  print(group_var)
  
  df %>%
    group_by(!! group_var) %>%
    summarise(a = mean(a))
}

my_summarise(df, g1)

# 1.3. Different input variable ----
# Compute three summaries, varying the input variable:
summarise(df, mean = mean(a), sum = sum(a), n = n())

summarise(df, mean = mean(a * b), sum = sum(a * b), n = n())

my_summarise2 <- function(df, expr) {
  expr <- enquo(expr)
  
  summarise(df,
            mean = mean(!! expr),
            sum = sum(!! expr),
            n = n()
  )
}

my_summarise2(df, a)

my_summarise2(df, a * b)

# 1.4. Different input and output variable ----
# Vary the name of the output variables:
mutate(df, mean_a = mean(a), sum_a = sum(a))
mutate(df, mean_b = mean(b), sum_b = sum(b))

# Using quo_name()
my_mutate <- function(df, expr) {
  expr <- enquo(expr)
  mean_name <- paste0("mean_", quo_name(expr))
  sum_name <- paste0("sum_", quo_name(expr))
  
  mutate(df,
         !! mean_name := mean(!! expr),
         !! sum_name := sum(!! expr)
  )
}

my_mutate(df, a)

# 1.5. Capturing multiple variables ----
# quos()
my_summarise <- function(df, ...) {
  group_var <- quos(...)
  
  # print(quos(!!! group_var))
  # print(quos(group_var))
  # print(group_var)
  
  df %>%
    group_by(!!! group_var) %>%
    summarise(a = mean(a))
}

my_summarise(df, g1, g2)

# With mutate now:
my_mutate <- function(df, ...) {
  l_vars <- quos(...)
  
  for (v in l_vars) {
    mean_name = paste0("mean_", quo_name(v))
    sum_name <- paste0("sum_", quo_name(v))
    
    df <- mutate(df,
           !! mean_name := mean(!! v),
           !! sum_name := sum(!! v))
  }
  df
}

my_mutate(df, a)
my_mutate(df, a, b)

# !!! takes a list of elements and splices them into to the current call. 
# Look at the bottom of the !!! and think ....

args <- list(na.rm = TRUE, trim = 0.25)
args
quo(mean(x, !!! args))

args <- list(quo(x), na.rm = TRUE, trim = 0.25)
args
quo(mean(!!! args))

# 2. Quoting ----

# Quoting is the action of capturing an expression instead of evaluating it.
# One of the most important quoting operators in R is the formula:
disp ~ cyl + drat

# The other quoting operator in base R is quote(). It returns a raw expression 
# rather than a formula:
# Computing the value of the expression:
toupper(letters[1:5])

# Capturing the expression:
quote(toupper(letters[1:5]))

# The formula is the better of the two options because it captures the code 
# and its execution environment.

f <- function(x) {
  quo(x)
}

x1 <- f(10)
x2 <- f(100)

library(rlang)

x1
x2

# But the expressions are not the same:

rlang::get_env(x1)
rlang::get_env(x2)

rlang::eval_tidy(x1)
rlang::eval_tidy(x2)

# One name can refer to different values in different environments. 

# This is also important for dplyr, because it allows you to combine variables 
# and objects in a call:

user_var <- 1000
mtcars %>% summarise(cyl = mean(cyl) * user_var)

# When an object keeps track of an environment, it is said to have an enclosure.
# For this reason we use a special name to refer to one-sided formulas: quosures. One-sided formulas are quotes (they carry an expression) with an environment.
# Quosures are regular R objects. They can be stored in a variable and inspected:
  
var <- ~toupper(letters[1:5])
var

# You can extract its expression:
rlang::get_expr(var)
#> toupper(letters[1:5])

# Or inspect its enclosure:
rlang::get_env(var)

# 3. Quasiquotation ----


# 3.1. Unquoting ----


# 3.2. Unquote-splicing ----