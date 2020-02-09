# spock-rest

This is me following the tutorial from here: https://www.spock.li/tutorials/rest-api

Goal: Get a grasp of creating a rest api with Haskell.

Here's also the description of my experience:

1. Setup of the project:

I am using IntelliJ with the Intellij-Haskell plugin with hopes that using a familiar editor will make learning unfamiliar things ever slightly less dreadful.
I had no problem creating a new stack project and opening it up in the editor and with [https://github.com/brynedwards/spock-tutorials/issues/1][help] from the author of the tutorial I could add the dependencies to the Cabal file just fine. 
The only problem I had here was that after adding the dependencies the sample Main file from stack broke and couldn't find the Lib module it depended on but it doesn't seem to be an urgent problem at the moment.
At this point in time I also realise that the spock-rest.cabal file is in .gitignore when a project is created through stack. After a bit of Googling I see that it can be generated via modifying package.yaml file and so far I have been directly modifying the Cabal file and now I;m not sure what's the best way to proceed. 
I will probably try posting those questions in the FP Slack.
Day 2: I posted my very first question in the haskell-beginners channel and got an immediate response, awesome! I also decided to use the package.yaml file instead of the cabal file.

[help]: https://github.com/brynedwards/spock-tutorials/issues/
