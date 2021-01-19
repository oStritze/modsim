wayhome <- function(maxiter, 
                    type = "C", 
                    end_if_back = FALSE, 
                    n_events = 100,
                    car_prob = 0.05){
  # maxiter ... iterations of simulation
  # type ... case "A", "B" or "C"
  # end_if_back ... boolean, if TRUE: the simulation stops if the person returns to y < 1
  # n_events ... initialize number of events for person moving
  # car_prob ... probability of car occuring
  ########################################################################################
  
  ## save result in list
  res_list <- vector("list", maxiter)
  
  for(i in 1:maxiter){
    ### when the events should occur
    ## for "C": exponential distribution is used with lambda = 1
    if(type == "C"){
      events <- cumsum(rexp(n_events, 1))
    } else {
      events <- 1:n_events
    }
    
    ####### initialize variables
    x <- 0
    y <- 1
    direction <- 90 # (calculate the position with sinus & cosinus) 
    # 90 (North), 0 (East), 180 (West), 270 (South) 
    
    ### number of car occurrences till the first event (person moving)
    ### car_1 is for lane 1 (y in (1, 3])
    ### car_2 is for lane 2 (y in (5, 7])
    car_1 <- sum(runif(floor(events[1])+1) < car_prob)
    car_2 <- sum(runif(floor(events[1])+1) < car_prob)
    
    ## initialize result matrix
    res <- matrix(c(events[1], x, y, car_1, car_2, direction, 0), 
                  ncol = 7, 
                  dimnames=list(NULL, c("time", "x", "y", "car_1", "car_2", "direction", "degree")))
    
    ## assuming the event of car can only occur with t is a natural number
    ## calculate how often the event of car occurs between two events of person moving
    disc_time <- diff(floor(events))
    
    ## Is the person hit or not? 
    if(car_1 != 0){
      hit_1 <- y > 1 & y <= 3
    } else {
      hit_1 <- FALSE
    }
    if(car_2 != 0){
      hit_2 <- y > 5 & y <= 7
    } else {
      hit_2 <- FALSE
    }
    
    ## whether the simulation should stop, if the person returns to y < 1
    if(end_if_back){
      y_min <- 1
    } else {
      y_min <- -Inf
    }
    
    for(j in 2:length(events)){
      if(y <= 7 & !hit_1 & !hit_2 & y >= y_min){
        ## degree change of the current direction
        if(type == "A") {
          degree <- sample(c(-90, 0, 90), 1, prob = c(0.25, 0.5, 0.25))
        } else {
          degree <- runif(1, -120, 120)
        }
        
        ## calculate new direction
        new_direction <- (direction + degree)%%360 
        x <- x + cospi(new_direction/180)
        y <- y + sinpi(new_direction/180)
        
        ## simulate events of car occurring during the event of person moving
        if(disc_time[j-1] != 0){
          car_1 <- sum(runif(disc_time[j-1]) < car_prob)
          car_2 <- sum(runif(disc_time[j-1]) < car_prob)
        }
        
        ## Is the person hit or not? 
        if(car_1 != 0){
          hit_1 <- y > 1 & y <= 3
        } else {
          hit_1 <- FALSE
        }
        if(car_2 != 0){
          hit_2 <- y > 5 & y <= 7
        } else {
          hit_2 <- FALSE
        }
        
        ## save results
        res <- rbind(res, c(events[j], x, y, car_1, car_2, new_direction, degree))
        
        ## update direction
        direction <- new_direction
      } else {
        
        ### break if condition not fullfilled
        break
      }
    }
    res_list[[i]] <- as.data.frame(res)
  }
  return(res_list)
}

test <- wayhome(1)

save_plots <- function(res, iter = 1){
  # res ... result of wayhome() function
  # iter .. iterations of simulation, which should be saved as png 
  ################################################################
  
  for(i in 1:iter)
    for(j in 1:nrow(res[[i]])){
      ## save png (proportional width and height)
      png(paste0("street_iter", i ,"_event", j, ".png"), 
          height = 400, 
          width = (ceiling(max(res[[i]]$x))-floor(min(res[[i]]$x)))*50)
      
      ## empty plot
      plot(res[[i]]$x, res[[i]]$y, 
           type = "n", 
           ylab = "Street", xlab = "", 
           ylim = c(0, 8),
           main = paste0("t = ", round(res[[i]]$time[j], 3)),
           xaxt = "n", yaxt = "n")
      ## plot save areas
      polygon(rep(c((min(res[[i]]$x)-1)*2, (max(res[[i]]$x)+1)*2), each = 2), y = c(-1, 1, 1, -1), col = "lightgreen")
      polygon(rep(c((min(res[[i]]$x)-1)*2, (max(res[[i]]$x)+1)*2), each = 2), y = c(3, 5, 5, 3), col = "lightgreen")
      polygon(rep(c((min(res[[i]]$x)-1)*2, (max(res[[i]]$x)+1)*2), each = 2), y = c(7, 9, 9, 7), col = "lightgreen")
      
      ## plot red area, if cars occur
      if(res[[i]]$car_1[j] != 0){
        polygon(rep(c((min(res[[i]]$x)-1)*2, (max(res[[i]]$x)+1)*2), each = 2), y = c(1, 3, 3, 1), col = "red")
      }
      if(res[[i]]$car_2[j] != 0){
        polygon(rep(c((min(res[[i]]$x)-1)*2, (max(res[[i]]$x)+1)*2), each = 2), y = c(5, 7, 7, 5), col = "red")
      }
      
      ## plot person
      points(c(0, res[[i]]$x[1:j]), c(0,res[[i]]$y[1:j]), pch = 20)
      lines(c(0, res[[i]]$x[1:j]), c(0,res[[i]]$y[1:j]))
      dev.off()
    }      
}

test <- wayhome(1)
save_plots(test)

### save as gif: system()-function or in terminal
#system("convert -delay 30 *.png example_1.gif")
