#' Internal function to get and downsample the delta data used in sync when sync_type = delta
#' @inheritParams getInpSync
#' @noRd
getToaDelta <- function(toa, inp_toa_list, keep_rate, time_keeper_idx, offset_vals, ss_vals){
		toa_delta <- as.matrix(expand.grid(h1=1:40, h2=1:40, ping_idx=1:nrow(toa)))
		toa_delta <- data.table::data.table(toa_delta[which(toa_delta[,2] > toa_delta[,1]), ])
		
		toa_delta[, t1 := toa[as.matrix(toa_delta[,c(3, 1)])]]
		toa_delta[, t2 := toa[as.matrix(toa_delta[,c(3, 2)])]]
		
		toa_delta <- toa_delta[!is.na(t1) & !is.na(t2)]
		toa_delta[, t_delta := t1 - t2]
		toa_delta[, t1 := NULL]
		toa_delta[, t2 := NULL]
		toa_delta <- toa_delta[, .(h1, h2, t_delta, ping_idx)]
		
		toa_delta[, sync_tag_idx 	:= inp_toa_list$sync_tag_idx_vec[as.matrix(toa_delta[, 'ping_idx'])]]
		toa_delta[, offset_idx 		:= offset_vals$offset_idx[as.matrix(toa_delta[, 'ping_idx'])]]
		toa_delta[, ss_idx 			:= ss_vals$ss_idx[as.matrix(toa_delta[, 'ping_idx'])]]
		
		
		
		num_h1 <- toa_delta[, .N, by=.(H=h1, offset_idx)]
		num_h2 <- toa_delta[, .N, by=.(H=h2, offset_idx)]
		
		
		
		nums <- merge(num_h1, num_h2, by=c('H', 'offset_idx'), all=TRUE)
		nums[is.na(N.x), N.x :=0]
		nums[is.na(N.y), N.y :=0]
		nums[, N := N.x + N.y]
		# sum(num_h2$N)
		
		# nums[offset_idx == 1]
		
		# ggplot(nums) + geom_tile(aes(x=offset_idx, y=H, fill=log10(N)))
		
		keeps <- c()
		for(i in 1:offset_vals$n_offset_idx){
			keeps_i <- c()
			
			nums_i <- nums[offset_idx == i]
			data.table::setorder(nums_i, N)
			
			for(h in 1:ncol(toa)){
				idx_ih <- with(toa_delta, which(offset_idx == i & (h1 == nums_i[h, H] | h2 == nums_i[h, H])))
				already_in_keeps <- sum(idx_ih %in% keeps_i)
				
				if(already_in_keeps >= keep_rate){
					next
				} else {
					need <- keep_rate - already_in_keeps + 1
					if(length(idx_ih) < need){
						keeps_i <- c(keeps_i, idx_ih)
					} else {
						keeps_i <- c(keeps_i, sort(sample(idx_ih, size=keep_rate)))
					}
				}
			}
			keeps <- c(keeps, keeps_i)
			
		}
	
		toa_delta <- as.matrix(toa_delta[keeps])
		
		return(toa_delta)
		

}
