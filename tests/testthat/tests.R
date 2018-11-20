mtcars_df <- mtcars
mtcars_df$cyl  <- factor(mtcars$cyl)
mtcars_df$vs   <- factor(mtcars$vs)
mtcars_df$am   <- factor(mtcars$am)
mtcars_df$gear <- factor(mtcars$gear)

context("lm models")
mod <- lm(mpg ~ ., mtcars_df)
p_cont <- mem_plot(mod, "wt")
p_disc <- mem_plot(mod, "cyl")
expect_s3_class(p_cont, "ggplot")
expect_s3_class(p_disc, "ggplot")

p_cont_built <- ggplot2::ggplot_build(p_cont)
p_disc_built <- ggplot2::ggplot_build(p_disc)
expect_s3_class(p_cont_built, "ggplot_built")
expect_s3_class(p_disc_built, "ggplot_built")


context("glm models")
mod <- glm(am ~ wt + cyl, "binomial", mtcars_df)
p_cont <- mem_plot(mod, "wt")
p_disc <- mem_plot(mod, "cyl")
expect_s3_class(p_cont, "ggplot")
expect_s3_class(p_disc, "ggplot")

p_cont_built <- ggplot2::ggplot_build(p_cont)
p_disc_built <- ggplot2::ggplot_build(p_disc)
expect_s3_class(p_cont_built, "ggplot_built")
expect_s3_class(p_disc_built, "ggplot_built")

context("lmer models")
mod <- lme4::lmer(mpg ~ (1 | cyl) + wt + qsec,  mtcars_df)
p_cont <- mem_plot(mod, "wt")
p_disc <- mem_plot(mod, "cyl")
expect_s3_class(p_cont, "ggplot")
expect_s3_class(p_disc, "ggplot")

p_cont_built <- ggplot2::ggplot_build(p_cont)
p_disc_built <- ggplot2::ggplot_build(p_disc)
expect_s3_class(p_cont_built, "ggplot_built")
expect_s3_class(p_disc_built, "ggplot_built")


context("glmer models")
mod <- lme4::glmer(am ~ (1 | cyl) + wt, mtcars_df, "binomial")
p_cont <- mem_plot(mod, "wt")
p_disc <- mem_plot(mod, "cyl")
expect_s3_class(p_cont, "ggplot")
expect_s3_class(p_disc, "ggplot")

p_cont_built <- ggplot2::ggplot_build(p_cont)
p_disc_built <- ggplot2::ggplot_build(p_disc)
expect_s3_class(p_cont_built, "ggplot_built")
expect_s3_class(p_disc_built, "ggplot_built")

