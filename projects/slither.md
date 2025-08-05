---
name: Slither
technologies:
  - lisp
  - guix
  - opengl
date: 01.08.2025
source-link: https://github.com/Fuglesteg/slither
---
    
# Slither

> A game engine focusing on making game development easy

Slither is a game engine written in Common Lisp. It's focus is on being easy to work with and interactive.

The project started as a learning opportunity for myself to learn about shaders and graphics programming. I thought it would be fun to make something like shader toy in Common Lisp. As the project grew I wanted to use it make a game and then eventually it turned into a game engine. The name Slither came from the fact that I wanted to make a snake like game so for a while the project was simply "snake game" before becoming "Slither".

The project is in very early stages, but it is already quite simple to use in my opinion. This is due to a few macros I have developed that smooth over a lot of implementation details. Both for user's of the engine but also for development of the engine itself. If i for example want to make a new built in shader for the engine I can use the same `defshader` that the users of the engine would use.

I made the first game using the engine for the 2025 Lisp game jam. The game ["Glob"](https://github.com/Fuglesteg/Glob) is a game similar to and inspired by Agar.io. I reused a shader I was planning to use for a screen saver. Not much time went into the game itself as a lot of things had to come together in the engine in the time of the jam. I was also working my full time job during the day and the national day in Norway, 17th of May, also took out a huge chunk of my development time. I am however very happy with the result considering the limitations. The game also lacks audio as Slither did not support it at the time.

Slither continues development, and hopefully for next years game jam I will have more time to focus on the game itself. For now the plan is to develop a small prototype for a multiplayer game idea I have.