#tower of monsters in R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));
# libraries
library(Rdice);
library(ggplot2);
#library(gganimate)


test <- read.csv("test.csv", sep = ",");

gamestate <- 0;


#mm <- ggplot(test, aes(y=y, x=x, frame=x)) + geom_path(aes(cumulative = TRUE)) + annotate(geom="text", x=25, y=50, label="The Endless Tower", size=13,
#         color="red");


CreatePlayer <- function()
{
  me <- list(
    Name = "",
    Level = 1,
    HP = 20,
    MaxHP = 20,
    AC = 10,
    Initiative = 2,
    TempInit = 0,
    aDice = 1,
    Experience=0,
    Gold = 0,
    nDice = 4
  )
  
  return(me);
}

#Main loop
while(1==1)
{
  #main menu - 0
  if(gamestate == 0)
  {
    cat("Welcome to the endless tower!\npress N for new game or Q to quit.\n");
    #plot(c(1,2),c(1,3));
    
    input <- readline();
    
    switch(input,
           N={
      cat("New game Selected!\nWhat is your name?\n");
      player <- CreatePlayer();
      
      player$Name <- readline();
      monsterdata <- read.csv("monsters.csv", sep = ",");
      gamestate <- 1;
    },
    Q={
      cat("Thank you for playing!");
      stop("Thank you for playing!");
    }
    );
  }
  
  #newgame/Town - 1
  if(gamestate == 1)
  {
    cat("You entered the town in front of the Tower.\nE - Enter Tower.\nS - Go to shop.\nT - Go to the Dojo.\nC - Go to the temple.\nQ1 - Returning to main menu.\nQ2 - Quit.\n");
    input <- readline();
    
    switch(input,
           E={
             cat("You enter the tower...\n");
             gamestate <- 2;
           },
           S={},
           T={},
           D={},
           C={
             gamestate <- 6;
           },
           Q1={
             cat("Returning to main menu.\n");
             gamestate <- 0;
             },
           Q2={
             stop("Thank you for playing.\n");
           }
           );
    
  }
  
  #Enter Tower - 2
  #turn order (based on initiative)
  if(gamestate == 2)
  {
    amount_of_monsters <- dice.roll(6,1,1);
    cat("You encounter ",as.integer(amount_of_monsters$results)," monsters...\n");
    
    #creating monsters
    monsters <- vector("list", as.integer(amount_of_monsters$results));
    rndnumbers <- floor(runif(as.integer(amount_of_monsters$results), min=1, max=nrow(monsterdata)+1));
    for(i in 1:as.integer(amount_of_monsters$results))
    {
      monsters[[i]] <- CreatePlayer();
      monsters[[i]]$Name =  as.character(monsterdata$Name[rndnumbers[i]]);
      monsters[[i]]$Level = monsterdata$Level[rndnumbers[i]];
      monsters[[i]]$HP = monsterdata$Hp[rndnumbers[i]];
      monsters[[i]]$AC = monsterdata$AC[rndnumbers[i]];
      monsters[[i]]$Initiative = monsterdata$Initiative[rndnumbers[i]];
      monsters[[i]]$aDice = monsterdata$aDice[rndnumbers[i]]+4;
      monsters[[i]]$nDice = monsterdata$nDice[rndnumbers[i]];
      monsters[[i]]$Experience = monsterdata$Experience[rndnumbers[i]];
      monsters[[i]]$Gold = monsterdata$Gold[rndnumbers[i]];
      
      
      cat("[",i,"] ", monsters[[i]]$Name, " Lv.", monsters[[i]]$Level, "\t");
    }
    cat("\nDo you FIGHT (Z) or FLEE (X)?");
    
    input <- readline();
    
    switch(input,
           Z={
             order <- vector("integer", (as.integer(amount_of_monsters$results)+1));
             for(n in 1:length(order))
             {
               t <-  dice.roll(20,1,1);
               if(n == length(order))
               {
                  player$TempInit = player$Initiative + as.integer(t$results);
                  order[[n]] <- player$TempInit;
               }
               else
               {
                  monsters[[n]]$TempInit = monsters[[n]]$Initiative + as.integer(t$results);
                  order[[n]] <- monsters[[n]]$TempInit;
               }
                
             }
             order <- sort(order, decreasing = TRUE);
             gamestate <- 3;
           },
           X={
             cat("You flee and succesfully reached town.\n");
             gamestate <- 1;
           }
           );
    
    
  }
  
  #combat - 3
  if(gamestate == 3)
  {
    #cleared floor
    if(length(monsters) ==0)
    {
      cat("You have cleared this floor!\nGood luck on the next one :)\n");
      gamestate <- 2;
    }
    else
    {
      #one Round
      for(t in 1:length(order))
      {
        if (is.null(player) || gamestate != 3)
        {
          break;
        }
        if(order[[t]] == player$TempInit)
        {
          cat("It is your turn.\nATTACK (Z) or FLEE(X)?\n");
          input <- readline();
          
          switch(input,
                 Z={
                   cat("Who do you attack? (Number)\n");
                   input <- readline();
                   input <- as.integer(input);
                   attackRoll <- dice.roll(20,1,1);
                   if(as.integer(attackRoll$results) >= monsters[[input]]$AC)
                   {
                     dmgRoll <- dice.roll(player$aDice, player$nDice,1);
                     dmg <- sum(as.integer(dmgRoll$results));
                     cat("You dealt ", dmg, " damage to [", input,"] ", monsters[[input]]$Name,".\n");
                     monsters[[input]]$HP <- monsters[[input]]$HP - dmg;
                     if (monsters[[input]]$HP <= 0)
                     {
                       cat("[", input,"] ", monsters[[input]]$Name," died.\n");
                       player$Experience <- player$Experience + monsters[[input]]$Experience;
                       player$Gold <- player$Gold + monsters[[input]]$Gold;
                       monsters[[input]] <- NULL;
                       if(length(monsters) ==0)
                       {
                         break;
                       }
                     }
                   }
                   else
                   {
                     cat("You have missed the target.\n");
                   }
                 },
                 X={
                   cat("You flee and succesfully reached town.\n");
                   gamestate <- 1;
                   break;
                   }
                 );
        }
        else
        {
          for(i in 1:length(monsters))
          {
            if(order[[t]] == monsters[[i]]$TempInit)
            {
              cat("[",i,"] ", monsters[[i]]$Name, "is trying to attack you!\n");
              
              attackRoll <- dice.roll(20,1,1);
              if(as.integer(attackRoll$results) >= player$AC)
              {
                dmgRoll <- dice.roll(monsters[[i]]$aDice, monsters[[i]]$nDice,1);
                dmg <- sum(as.integer(dmgRoll$results));
                cat("[",i,"] ", monsters[[i]]$Name, " dealt ", dmg, " damage to you.\n");
                player$HP <- player$HP - dmg;
                if (player$HP < 0)
                {
                  player <- NULL;
                  cat("You have died to an ", monsters[[i]]$Name, ". silly, silly...\n");
                  break;
                }
              }
              else
              {
                cat("[",i,"] ", monsters[[i]]$Name, " missed you.\n");
              }
            }
          }
        }
      }
      #initiate next round
      gamestate <-3.5;
    }
   
  }
  #between combat turns -3.5
  if(gamestate == 3.5)
  {
    if (is.null(player))
    {
      gamestate <- 0;
    }
    else
    {
      gamestate <- 3;
    }
  }
  
  #Shop - 4
  
  #Dojo - 5
  #Temple - 6
  if(gamestate == 6)
  {
    cat("You went to the temple to check your health.\n\nWhen the priests looks at you, he tells you:\n",
        "Name: ", player$Name,"\n",
        "HP: ", player$HP,"/",player$MaxHP,"\n",
        "Level: ", player$Level,"\n",
        "Armor Class: ", player$AC, "\n",
        "Attack: ", player$aDice, "x", player$nDice, "\n",
        "Gold: ", player$Gold, "\n\n",
        "Would you want to HEAL (Z) or LEVEL UP (X) or go back to town (Q).\n");
    input <- readline();
    switch(input,
           Z={
             if(player$HP < player$MaxHP)
             {
               goldcost <- 1 * (player$MaxHP-player$HP);
               cat("The priester can heal you with a blessing.\n","(Q) Go back, (Z) 1Hp for 1gp or (A) max HP for ", goldcost,"\n");
               input <- readline();
               switch(input,
                      Z={
                        if(player$HP < player$MaxHP)
                        {
                          player$HP <- player$HP +1;
                          player$Gold <- player$Gold - 1;
                        }
                        gamestate <- 6;
                      },
                      A={
                        player$Gold <- player$Gold - goldcost;
                        player$HP <- player$MaxHP;
                        gamestate <- 6;
                      },
                      Q={
                        cat("You go back to town.\n");
                        gamestate<-1;
                      }
                      );
             }
             else
             {
               cat("You are at full health.\n");
             }
           },
           X={
             if(player$Experience >= 1.5*player$Level)
             {
               player$Level <- player$Level + 1;
               cat("You leveled up from ", player$Level-1, " to ", player$Level, "\n");
               gamestate <- 6;
             }
             else
             {
               cat("You don't have enough experience.\n");
               gamestate <- 6;
             }
           },
           Q={
             cat("You go back to the town.\n");
             gamestate <- 1;
           });
  }
}
