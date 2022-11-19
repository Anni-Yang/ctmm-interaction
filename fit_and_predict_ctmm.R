library(ctmm)
library(data.table)
library(parallel)
sink("ctmm.log")

## Convert each pig trajectory to 5 minute CTMM trajctory using the ctmm package
## Save the predictions from CTMM on 5 minute scale

fit_ctmm_model = function(id, dat, interp_vals){

	## Fit CTMM model for each pig

	cat("Working on individual", id, "\n")

	tdat = dat[individual_ID == id]
	tdat = tdat[, .(date_time, longitude, latitude)]
	colnames(tdat) = c("timestamp", "location.long", "location.lat")
	tdat = tdat[order(timestamp), ]
	tdat = tdat[!duplicated(timestamp)]

	# Convert to telemetry object for use in CTMM
	crs_str = "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
	telm = as.telemetry(tdat, projection=crs_str)

	# Fit with CTMM
	guess = ctmm.guess(telm, interactive=F)
	fit = ctmm.fit(telm, guess)

	# Predict 5 minute intervals (60*5 seconds)
	mint = min(telm$t)
	maxt = max(telm$t)
	ind = (interp_vals >= mint) & (interp_vals <= maxt)
	newt = interp_vals[ind]

	pred = predict(fit, data=telm, t=newt)
	pred_dat = data.table(id=id, x=pred$x, y=pred$y, 
						  time_s=pred$t)
	fwrite(pred_dat, paste0("../data/ctmm_data/", "traj_", id, ".csv"))
	cat("Completed individual", id, "\n")


}

dat = fread("../movements.csv")
dat[, datetime:=as.POSIXct(strptime(date_time, format="%Y-%m-%d %H:%M:%S", tz="GMT"))]
dat[, t:=as.numeric(datetime)]
unq_collars = unique(dat$individual_ID)

mint = min(dat$t)
maxt = max(dat$t)

# Time is on the second scale
interp_vals = seq(mint, maxt, by=60*1)

mclapply(unq_collars, fit_ctmm_model, dat, interp_vals, mc.cores1)
sink()


