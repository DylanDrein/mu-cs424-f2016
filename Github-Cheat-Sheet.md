To contribute to this repo first click Fork at the right top of the projects Github page. This adds a copy of the repo to your own account. You can then clone your copy to your computer with
````
git clone https://github.com/ (your github username) /mu-cs424-f2016
````
Move into the folder with all the cloned files.
````
cd mu-cs424-f2016
````
Add your new files here.

If you're not experienced with markdown you can use the Atom editor by github which has the option to add a markdown preview view under Packages>Markdown Preview.  
[Here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) is a link to a detailed markdown reference sheet which may be handy.

Make changes locally to the files/add new files then run
````
git add -A
````
to add all files to git. To check everything is working.
````
git status
````
Where the files in green are ready to be pushed. Next run
````
git commit -m "commit message"
````
to give a brief summary of what you changed. Finally run  
````
git push
````
to take the changes made locally and publish them on Github.  

Now go back to the repo on your Github and click on new pull request near the top. You should see a page detailing the changes made to current files and the new files you added locally compared to the current class repo.
