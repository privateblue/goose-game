## Building and running the game

You must have Java and SBT installed, prior to building the project.

After cloning the repository, please run this command:
```
sbt assembly
```
This builds a runnable JAR file, and also runs all the unit tests.

Run the game with this command:
```
java -jar target/scala-2.13/goose.jar
```
Theoretically, it's possible to run the game using SBT:
```
sbt run
```
In practice though, it messes with the console. Therefore my recommendation is to opt for the former, and rather build and run the JAR.

#### Prank option

To turn on the Prank option in the game, include the `-withPrank` flag in the run command:
```
java -jar target/scala-2.13/goose.jar -withPrank
```

## Playing the game

Please see the [original README](https://github.com/xpeppers/goose-game-kata) for a general descriiption of the game. The game starts automatically after the configured number of players is reached by adding new players.

#### Prank option

In this current implementation, the prank does not apply to special moves, like bounce, bridge or stepping on a goose.
