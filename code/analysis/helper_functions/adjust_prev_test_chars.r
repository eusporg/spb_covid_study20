# Function that adjusts the prevalence
# estimates for test sensitivity and specificity
# in classical way
# We require a specific version of GJRM:
# remotes::install_version("GJRM", "0.2-2")
library(GJRM)

adjust_prev_test_chars <- function(prev_object, sensitivity = 1, specificity = 1) {

	# Debug: prev_object <- prev(out.basic.iga_g_m.param$fit, type = "naive", delta = T); sensitivity = 0.99; specificity = 0.98

	if(!is.na(prev_object$sim.prev)) {

		stop("Test quality adjustment is available only for delta method-computed prevalence confidence intervals")

	}
	
	# Extract SEs from CI bounds
	point_est <- prev_object$res[2]
	lb <- prev_object$res[1]
	qz <- qnorm(prev_object$prob.lev/2, lower.tail = F)

	sv <- (point_est - lb)/qz

	# Adjust point estimates and SEs for test characteristics
	point_est_adj <- (point_est + specificity - 1)/(sensitivity + specificity - 1)

	sv_adj <- sv/(sensitivity + specificity - 1)
	
	# Calculate new upper/lower b
	lb_adj <- point_est_adj - qz * sv_adj
	ub_adj <- point_est_adj + qz * sv_adj

	# Output
	res <- c(lb_adj, point_est_adj, ub_adj)
	out <- list(res = res, sv = sv_adj, prob.lev = prev_object$prob.lev, sim.prev = NA, sensitivity = sensitivity, specificity = specificity)
	class(out) <- "prev"
	out

}
