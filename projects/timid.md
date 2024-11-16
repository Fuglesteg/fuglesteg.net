---
name: Timid
source-link: https://github.com/fuglesteg/timid
technologies:
    - go
    - docker
    - github
---

# Timid

> The shy container

Timid is a UDP proxy server that puts Docker containers to sleep when no
connections are active. The interest for this project emerged as I was
attempting to host a [Valheim](https://valheimgame.com) server for some friends.
I ran the server using the excellent docker container
[lloesche/valheim-server](https://hub.docker.com/r/lloesche/valheim-server), I
did however notice that the server seemed to use a lot of resources even when no
one was connected to it (both CPU and memory). Now, my server doesn't have that
many resources so I was a little hesitant of hosting the valheim server if it
meant that it would slow down all my other services and increase the power draw.
I considered just manually turning on or off the server myself, but that defeats
the point of having a dedicated server. Also my friends would not be able to
play without me unless I specifically left the server running.
I also considered building a small website or similar that would allow my
friends to manually turn the server on, but this seemed kind of hacky.

On the server I also host minecraft servers using the excellent
[itzg/minecraft-server](https://hub.docker.com/r/itzg/minecraft-server) in
combination with the just as excellent [Infrared](https://infrared.dev/). Using
Infrared I can configure it to automatically start and stop Docker containers
when connections are made. This is great as it means that I can host many
servers at the same time and only the ones that are actually being used will use
any resources.

Wouldn't it be excellent to build a similar tool that can work for any type of
server hosted in docker? That's how I was inspired to make Timid, which does
basically the same except for UDP based servers. In addition to stopping and
starting containers you can also pause/unpause containers, meaning that you can
circumvent the long startup times of certain game servers at the cost of memory
consumption. You could also pause the container for a customisable amount of
time and then stop it if nobody connects to it.

Building Timid taught me a lot, it was the first project I made using Go that
wasn't a web API. It also required me to learn a lot about Docker and
controlling Docker containers using the Docker API. I mostly learned by reading
the great introductory book [Docker Deep Dive](https://www.amazon.com/Docker-Deep-Dive-Nigel-Poulton/dp/1916585256/).
