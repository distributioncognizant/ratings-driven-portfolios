####### Data ingest code and utilities to remain private
gatherData <- function(addresses_and_permissions){}

# Data corruption test
compareData <- function(new_data, old_data){}

# Auto-generated warning e-mails to PMs
dataAlert <- function(addresses_and_permissions){}




####### Analytical pseudocode


### Main procedure

load(old_data)

new_data <- gatherData(addresses_and_permissions)

data_consistency_test <- compareData(new_data, old_data)

if(data_consistency_test==TRUE){


  for(user in user_list){
    for(firm in firm_list){
      if(rated==TRUE){
        # exponentially weighted moving average, tunable to dampen signal turnover
        ewma_user_firm_ratings <- ewma(ratings(user,firm))
        # standard deviation
        sd_user_firm_ratings <- sd(ratings(user,firm))
        # dampen signal with respect to user consistency
        adjusted_ratings[user, firm] <- ewma_user_firm_rating/sd_user_firm_rating
      }

      else{adjusted_ratings[user, firm] <- NULL}
    }
  }

  for(firm in firm_list){
    # require that a certain threshold of reviews has been met
    if(count(adjusted_ratings[user, firm]>=threshold)){

      for(t in lookback_window){
        # dampen signal w.r.t. consistency across users in previous step
        adjusted_firm_rating_t <- mean(adjusted_ratings[all_users, firm, t])/
          sd(adjusted_ratings[all_users, firm, t])
        adjusted_firm_ratings[t] <- adjusted_firm_rating_t
      }

      # exponentially weighted moving average, tunable to dampen signal turnover
      ewma_firm_ratings <- ewma(adjusted_firm_ratings)
      sd_firm_ratings <- sd(adjusted_firm_ratings)
      # dampen signal with respect to firm consistency over time and market cap
      firm_ratings[firm] <- benchmark_weight[firm]*ewma_firm_ratings/sd_firm_ratings
    }

    # ignore firm in portfolio construction if insufficiently reviewed
    else{
       firm_ratings[firm] <- NULL
    }
  }

  # portfolio weight construction
  mean_rating <- mean(firm_ratings)
  rating_sd <- sd(firm_ratings)
  centered_ratings <- firm_ratings - mean_rating
  # active weightings now have mean 0, standard deviation 1
  standardized_ratings <- centered_ratings/rating_sd
  # trim any position in excess of maximum single name active weight
  final_weights <- windsorize(standardized_ratings, position_limit)


}
if(data_consistency_test==FALSE){
  dataAlert
}
