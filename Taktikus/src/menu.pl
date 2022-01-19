% dificulty(+Code, -Difficulty) returns the difficulty of the game associated with a code.
dificulty(1, 'Easy').
dificulty(2, 'Normal').

% logo/0, prints the game logo.
logo:-
    write('  ########   ###     ##    ##   ##  ######## ##     ##    #######   \n'),
    write('     ##     ## ##    ##   ##    ##     ##    ##     ##   ##    ##   \n'),
    write('     ##    ##   ##   ##  ##     ##     ##    ##     ##   ##         \n'),
    write('     ##   ##     ##  ####       ##     ##    ##     ##    #####     \n'),
    write('     ##   #########  ##  ##     ##     ##    ##     ##        ##    \n'),
    write('     ##   ##     ##  ##   ##    ##     ##    ##     ##  ##    ##    \n'),
    write('     ##   ##     ##  ##    ##   ##     ##     #######    ######     \n').


 
