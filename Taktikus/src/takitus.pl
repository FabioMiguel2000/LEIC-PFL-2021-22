% consults all necessary files, allowing functions to be used accros diffrent files.

:- use_module(library(lists)).
:- use_module(library(between)).

:- consult('view.pl').
:- consult('board.pl').
:- consult('utils.pl').
:- consult('moves.pl').
:- consult('menu.pl').
