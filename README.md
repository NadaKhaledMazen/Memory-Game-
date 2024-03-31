# Memory Game using AT89C51 Assembly Language

## Overview
This repository contains the implementation of a leveled memory game using assembly language for the AT89C51 microcontroller. The game features LED and button interaction and includes a timer function for each level.

## Features
- Leveled memory game logic
- LED and button interaction
- Timer functionality
- LCD display for level and score feedback

## Repository Structure
- **src/:** Contains the assembly source code files.
- **images/:** Contains project-related images.
  - **flowchart.png:** Flowchart of the project.
  - **schematic.png:** Schematic diagram of the hardware setup.
- **docs/:** Additional documentation files.

## Game Logic
The memory game follows a simple yet engaging logic:

- **Level Progression:** The game consists of multiple levels, each with an increasing difficulty. The player starts from level 1 and progresses through higher levels as they successfully complete each level.
- **LED Display:** LEDs are used to represent the patterns that the player needs to match. The LEDs will display a sequence of patterns that the player needs to remember and replicate.
- **Button Interaction:** The player interacts with the game using buttons connected to the microcontroller. They need to press the buttons in the correct sequence to match the displayed pattern.
- **Timer Functionality:** A timer starts as soon as the pattern is displayed. The player has a limited time (9 seconds) to replicate the pattern. If they fail to match the pattern within the given time, the game ends.
- **Score Management:** The player's score is determined by their performance in matching the patterns. If the player successfully matches a pattern within the time limit, their score increases. However, if they fail to match a pattern, they lose points. The game ensures that the player cannot lose if their score reaches a certain threshold (12 points), but it will return to a specified level.
- **Game Outcome:** If the player successfully completes all levels, they win the game. Otherwise, if they fail to complete a level within the time limit, they receive a "Game Over" message.

The game provides an enjoyable challenge for players to test and improve their memory and reaction skills.

## How to Use
1. Clone the repository to your local machine.
2. Assemble the assembly code using a suitable assembler.
3. Program the compiled binary onto the AT89C51 microcontroller.
4. Connect the necessary peripherals as per the schematic diagram.
5. Power up the microcontroller and play the memory game!

## Flowchart

![Flow Chart](https://github.com/NadaKhaledMazen/Memory-Game-/assets/105931027/be66aca4-4d81-48be-a4b9-37e8b305323e)

## Schematic Diagram

![Schematic](https://github.com/NadaKhaledMazen/Memory-Game-/assets/105931027/a2174c86-5822-4cb9-8483-b49c5735711a)
