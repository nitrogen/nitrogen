# Nitrogen Web Framework for Erlang

Nitrogen is an Erlang-based web framework that allows you to develop
infinitely scaleable, Ajax-rich web applications using a pure Erlang 
technology stack.

See the [Nitrogen Project website](http://nitrogenproject.com) for
additional information.

### Getting Started

Download the code and move to the experimental branch.

    git clone git://github.com/rklophaus/nitrogen.git
    cd nitrogen

Then, run the following...

### On Mac OSX / Linux

    cd Quickstart 
    ./quickstart.sh

Open your web browser to http://localhost:8000

### Create a New Project

    make rel_inets
    cd rel/nitrogen
    bin/nitrogen start

Open your web browser to http://localhost:8000