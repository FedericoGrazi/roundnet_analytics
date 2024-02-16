
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

generate_dens2d <- function(x, seed = 1234, players = c("P1","P2","P3","P4"), jitter_var = 0.15, min_dens = 0.08, adj_max = 0,
                            label = NULL, palette = 7, save = FALSE, PATH = "YOUR_PATH", subtitle = NULL){
  
    
  
  df <- 
    x %>% 
    # Select Players your are interested in
    filter(NameSet %in% players) %>% 
    # Create coords in ggplot circle
    mutate(coords1 = 
             case_when(
               Set == 1 ~ -.25,
               Set == 2 ~ -.25,
               Set == 3 ~ -.75,
               Set == 4 ~ -1.25,
               Set == 5 ~ -1.75,
               Set == 6 ~ -1.75,
               Set == 7 ~ -1.25,
               Set == 8 ~ -.75,
             ),
           coords2 = 
             case_when(
               Set == 1 ~ .75,
               Set == 2 ~ 1.25,
               Set == 3 ~ 1.75,
               Set == 4 ~ 1.75,
               Set == 5 ~ 1.25,
               Set == 6 ~ .75,
               Set == 7 ~ .25,
               Set == 8 ~ .25
             )) %>% 
    group_by(Plays) %>% 
    # Add random noise. I found that a variability of 0.15 works pretty good.
    # jitter_var can be adjusted by trial and errore
    mutate(
      coords1 = coords1 + rnorm(1,0,jitter_var),
      coords2 = coords2 + rnorm(1,0,jitter_var),
    )
  
  # Create actual circle
  dat <- circleFun(c(1,-1),2.3,npoints = nrow(df))
  
  # Set seed for having always the same random jitter OR to change the jitter itself.
  set.seed(seed)
  
  p <- ggplot(df, aes(coords2,coords1)) +
    # Density plot
    stat_density_2d(aes(fill = ..density..), geom = "raster",contour = F,show.legend = F)+
    # Add Circle
    geom_path(aes(dat$x,dat$y),col = "black", size = 2)+
    # Add fill
    scale_fill_distiller(palette = palette, # 7 is Red. I think it suites pretty well the plot
                         direction = 1, 
                         # Positions that have no points should be white. Hence a limits is needed
                         limits = c(min_dens, # Usually a small value near 0 removes it.
                                    # Given that limits are provided, also a maximum shall be provided
                                    max(apply(MARGIN = 1,
                                              X = as.data.frame(MASS::kde2d(df$coords1,df$coords2)$z), # Compute the maximum of the density
                                              FUN = max)+adj_max) # I dont know why but sometimes the maximum is off by little. So I added a value to adjust for this error.
                                    # You should add tiny amounts until you see that the red on the maximum density point is full red.
    ),
    na.value = "white") +
    theme_classic()+
    annotate("text", x = 1, y = .4,size = 9, label = "Server", col = "black")+
    annotate("text", x = 1, y = .4,size = 9, label = "Server", col = "black")+
    xlab("")+ylab("")+
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.background = element_rect(fill = "white")
    )+
    ggtitle(
      str_c("Setting Hot Zone for ", label), # Add title
      subtitle = subtitle # Add subtitle, if wanted.
    )
  
  # Save the Plot if you desire
  # If you want to save, also a PATH shoul be provided. 
  if(save == T) ggsave(str_c(PATH,"/", label,".png"), p, width = 10, height = 11)  
  
  return(p)
  
}


compute_stats <- function(x, sets = c("G1","G2","G3"),group_by = c("player","team"), Team1 = "Team1",Team2 = "Team2"){
  
  
  # Filter for any set you desire
  x = x %>% 
    filter(Game %in% sets)
  
  # General Descriptive Statistics
  all_serves <- 
    x %>% 
    group_by(NameS) %>% 
    summarize(TotServes=n()) %>% 
    filter(NameS != "No")
  
  all_hits <-
    x %>% 
    group_by(HitN) %>% 
    summarize(TotHits=n()) %>% 
    filter(HitN != "No")
  
  all_passes <- 
    x %>% 
    group_by(NameSet) %>%
    summarize(n=n()) %>% 
    filter(NameSet != "No")
  
  all_receptions <- 
    x %>% 
    group_by(NameR) %>%
    summarize(n=n()) %>% 
    filter(NameR != "No")
  
  # Serve Index ####
  
  ## SEP
  
  ### Aces
  aces <- 
    x %>% 
    group_by(NameS) %>% 
    summarize(ace = sum(ACE)) %>% 
    merge(all_serves) %>% 
    mutate(`Ace%` = ace/TotServes) %>% 
    filter(NameS != "No")
  
  
  ### Bad Reception Conversion
  bad_reception <- 
    x %>% 
    group_by(NameS) %>% 
    filter(Receive != 2, ACE != 1, Serve != 0, Set == 0) %>% 
    summarize(n=n()) # None - a bad set that doesn't let the hitter attack is counted still as a set or is it labeled 0?
  
  ## DFP
  dfp <- 
    x %>% 
    group_by(NameS) %>% 
    filter(Serve == 0) %>% 
    summarize(faults=n()) %>% 
    merge(all_serves) %>% 
    mutate(DFP = faults/TotServes ) %>% 
    filter(NameS != "No")
  
  
  I_service = aces$`Ace%` * (1 - dfp$DFP)
  
  # Offence Index ####
  
  ## KP
  kp <- 
    x %>% 
    group_by(HitN) %>% 
    filter(Converted == 1) %>% 
    summarize(kills=n()) %>% 
    merge(all_hits) %>% 
    mutate(KP = kills/TotHits ) %>% 
    filter(HitN != "No")
  
  
  ## PP
  pp <- 
    x %>% 
    group_by(NameSet) %>% 
    filter(Set != 0) %>% 
    summarize(not_err = n()) %>% 
    merge(all_passes) %>% 
    mutate(pp = not_err/n) %>% 
    filter(NameSet != "No")
  
  
  I_offence = kp$KP+ pp$pp*2/3
  
  # Defense Index ####
  
  ## RP
  rp <- 
    x %>% 
    group_by(NameR) %>% 
    filter(Receive != 0 ) %>% 
    summarize(reception = n()) %>% 
    merge(all_receptions) %>% 
    mutate(rp = reception/n) %>% 
    filter(NameR != "No")
  
  ## DEP
  hits_team1 <- 
    x %>% 
    filter(HitN %in% c("P1","P2")) %>% 
    summarize(n=n()) %>% 
    as.integer()
  
  hits_team2 <- 
    x %>% 
    filter(HitN %in% c("P3","P4")) %>% 
    summarize(n=n()) %>% 
    as.integer()
  
  dep <- 
    x %>% 
    group_by(DefenseTN) %>% 
    summarize(def = n()) %>% 
    filter(DefenseTN != "No") %>% 
    mutate(hits_opp = case_when(
      DefenseTN == "P1" ~ hits_team2,
      DefenseTN == "P2" ~ hits_team2,
      DefenseTN == "P3" ~ hits_team1,
      DefenseTN == "P4" ~ hits_team2
    ),
           dep = def/hits_opp) %>% 
    filter(DefenseTN != "No")
  
  if(length(dep$DefenseTN)<4){
  pl = c("P1","P2","P3","P4")
  
  nodef = data.frame(
    DefenseTN = pl[is.na(match(pl,dep$DefenseTN))],
    dep = rep(0,length(pl[is.na(match(pl,dep$DefenseTN))]))
  )
  
  dep = rbind(dep %>% select(DefenseTN, dep), nodef) 
  }

  
  I_defence = .2 * (4 * rp$rp + dep$dep)
  
  
  ## rpr
  rpr = (I_service + I_offence + I_defence)*100/3
  
  
  stats <- data.frame(
    Kills = kp$KP,
    Ace = aces$`Ace%`,
    Serves = 1-dfp$DFP,
    Receive = rp$rp,
    Defence = dep$dep,
    RPR = rpr/100
  )
  
  stats <-
    stats %>% 
    mutate_all(function(x) round(x*100,1)) %>% 
    as.data.frame
  
  if(group_by == "teams"){
    stats <- 
      stats %>% 
      mutate(teams = c(1,1,2,2)) %>% 
      group_by(teams) %>% 
      summarize_all(function(x) mean(x)) %>% 
      select(-teams) %>% 
      mutate(team = c(Team1,Team2)) %>% 
      relocate(team, .before = "Kills") %>% 
      as.data.frame
  }
  
  print("Attention! All values are returned as Percentages!")
  return(stats)
  
}


ball_body_plot <- function(x, players = c("P1","P2","P3","P4"), type = c("density","histogram"), seed = 1451,path ="D:/Personal Statistics/[RCB] - Roundnet Analytics//body arms.png" ){
  
  
  mypng <- readPNG(path)
  
  
  set.seed(seed)
  
  if(type == "density"){
  p <- 
    x %>% 
    filter(NameSet %in% players) %>% 
    group_by(Plays) %>% 
    # Add random noise
    mutate(height =Höhe+ rnorm(1,0,0.5),
           height = ifelse(height< 0,height+rnorm(1,1.5,0.2),height )) %>% 
    ggplot(aes(height))+
    # Plot Density
    geom_density(col = "black")+
    # Add Standing Man Image
    annotation_raster(mypng, ymin = -.25,ymax= 0.05,xmin = 0,xmax = 3.5) +
    # Add Line for visualization
    geom_hline(yintercept = 0.038, col = "black")+
    coord_flip()+
    ylab("Set Distribution")+
    # Adjust the size of the image
    xlim(c(0.3,4.5))+ylim(c(-.2,.8))+
    # Remove everything else
    theme_classic()+
    xlab("")+
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      plot.background = element_rect(fill = "white")
    )
  } else {
  
  p <-
    x %>% 
    filter(NameSet %in% players) %>% 
    group_by(Plays) %>% 
    mutate(height =Höhe+ rnorm(1,0,0.5),
           height = ifelse(height< 0,height+rnorm(1,1.5,0.2),height )) %>% 
    ggplot(aes(height))+
    geom_histogram(bins = 20, fill = NA, col = "black")+
    annotation_raster(mypng, ymin = -7,ymax=-.3,xmin = 0,xmax = 3.2) +
    coord_flip()+
    xlim(c(0.1,4))+ylim(c(-7,20))+
    ylab("Set Count")+
    theme_classic()+
    xlab("")+
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      plot.background = element_rect(fill = "white")
    )
    
  }
  
  p <- p+ggtitle("Distribution of the Height of the Set")
  
  return(p)
}