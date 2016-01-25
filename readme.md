Andrew Fairless
December 2015

The project below was created to produce word clouds from text messages that
were forwarded from cell phone to email with the app 'txtForward'.

These emails were exported from Mozilla Thunderbird into a '.csv' file.
To protect privacy, the text message contents in the example 'messages.csv'
file has been replaced with lines from Shakespeare's 'The Comedy of Errors'.
The file 'messages.csv' also contains fake dates/times and a fake email address.

The functions below were created to retain (some) punctuation (for, e.g., 
emoticons) and manual control over some 'text cleaning' processes, instead of 
using only the functions provided in the library 'tm'.

imports:  text messages from 'messages.csv' in current working directory
exports:  'png' and 'pdf' files of word clouds constructed from the text messages
dependencies:  libraries 'tm' and 'wordcloud'
