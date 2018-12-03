claims_input <- readr::read_lines("data/day03_1_input.txt") 

# How many square inches of fabric are within two or more claims?
# 1. Transform Raw Data for calcuating 
df <- data.frame(raw_data = claims_input,
                 stringsAsFactors = FALSE) %>%
      separate(raw_data, into = c("claim_id", "step1"), sep = " @ ", convert = TRUE) %>%
      separate(step1, into = c("distance_leftedge", "step2"), sep = ",", remove = TRUE, convert = TRUE) %>%
      separate(step2, into = c("distance_topedge", "step3"), sep = ": ", remove = TRUE, convert = TRUE) %>%
      separate(step3, into = c("wide", "tall"), sep = "x", remove = TRUE, convert = TRUE)


# Testdata
#df <- data.frame(claim = c(1,2,3),
#                 distance_leftedge = c(1,3,5),
#                 distance_topedge = c(3,1,5),
#                 wide = c(4,4,4),
#                 tall = c(4,4,2))

#df <- data.frame(claim = c(123),
#                 distance_leftedge = c(3),
#                 distance_topedge = c(2),
#                 wide = c(5),
#                tall = c(4))


number_claims <- nrow(df)

# 2. Step: Calculate all fields for a claim in tidy-way

claims <- data.frame()
for(i in 1:number_claims){
  
  claim_col <- seq((df$distance_leftedge[i] + 1), (df$distance_leftedge[i]+df$wide[i]))
  claim_row <- seq((df$distance_topedge[i] + 1), (df$distance_topedge[i]+df$tall[i]))
  
  dd <- 
    crossing(claim_row, claim_col) %>%
    mutate(id = paste0(claim_row, "_", claim_col),
           claim = df$claim[i])
  
  claims <- bind_rows(claims, dd)
}

# 3. Step find all fields with more than one Claim
field_id <- 
  claims %>% 
  group_by(id) %>%
  summarise(number_claims = n()) %>%
  ungroup()

field_id %>%
  filter(number_claims > 1) %>%
  nrow()  

# Part 2: Find the field with no overlapping: Claim with number fields == number fields with only one clam as owner

# Number of Square Inches
claim_id <- 
  claims %>%
  group_by(claim) %>%
  summarise(number_fields = n()) %>%
  ungroup()
  
dd <- 
  claims %>%
  left_join(field_id, by = "id") %>%
  filter(number_claims == 1) %>%
  group_by(claim) %>%
  summarise(number_unique_fields = n()) %>%
  ungroup()

dd %>%
  left_join(claim_id, by = "claim") %>%
  filter(number_unique_fields == number_fields)




