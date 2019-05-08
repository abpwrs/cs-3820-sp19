These instructions were tested on Mojave 10.14.2. There should only be minor differences for other OSes, if any.

1. Install GitKraken. https://www.gitkraken.com/download
2. Create a profile in GitKraken. Use your hawk id and uiowa email for the fields “Name” and “Email”.
3. Login to your UIowa Github account at github.uiowa.edu. Go to Settings -> Developer Settings -> Personal Access Tokens -> Generate New Token. Check all boxes and click “Generate Token”. Copy the token to your clipboard.
4. Return to GitKraken. Go to Preferences -> Authentication -> Github Enterprise, and connect with the token on your clipboard.
5. Now you should see your hawk id and the phrase “Connected” on the authentication page.
6. Click “Generate SSH key and add to Github Enterprise”.

At this point GitKraken should be successfully configured. To clone a repository, click the file icon on the upper left of GitKraken UI, then click “clone”.


See also [these notes](https://wiki.uiowa.edu/display/githubdocs/Frequently+Asked+Questions) from ITS.
