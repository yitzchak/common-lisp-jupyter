// leave at least 2 line with only a star on it below, or doc generation fails
/**
 *
 *
 * Placeholder for custom user javascript
 * mainly to be overridden in profile/static/custom/custom.js
 * This will always be an empty file in IPython
 *
 * User could add any javascript in the `profile/static/custom/custom.js` file
 * (and should create it if it does not exist).
 * It will be executed by the ipython notebook at load time.
 *
 * Same thing with `profile/static/custom/custom.css` to inject custom css into the notebook.
 *
 * Example :
 *
 * Create a custom button in toolbar that execute `%qtconsole` in kernel
 * and hence open a qtconsole attached to the same kernel as the current notebook
 *
 *    $([IPython.events]).on('app_initialized.NotebookApp', function(){
 *        IPython.toolbar.add_buttons_group([
 *            {
 *                 'label'   : 'run qtconsole',
 *                 'icon'    : 'icon-terminal', // select your icon from http://fortawesome.github.io/Font-Awesome/icons
 *                 'callback': function () {
 *                     IPython.notebook.kernel.execute('%qtconsole')
 *                 }
 *            }
 *            // add more button here if needed.
 *            ]);
 *    });
 *
 * Example :
 *
 *  Use `jQuery.getScript(url [, success(script, textStatus, jqXHR)] );`
 *  to load custom script into the notebook.
 *
 *    // to load the metadata ui extension example.
 *    $.getScript('/static/notebook/js/celltoolbarpresets/example.js');
 *    // or
 *    // to load the metadata ui extension to control slideshow mode / reveal js for nbconvert
 *    $.getScript('/static/notebook/js/celltoolbarpresets/slideshow.js');
 *
 *
 * @module IPython
 * @namespace IPython
 * @class customjs
 * @static
 */

$([IPython.events]).on('notebook_loaded.Notebook', function(){
    // add here logic that should be run once per **notebook load**
    // alert(IPython.notebook.metadata.language)
    IPython.notebook.metadata.language = 'common-lisp' ;
});
$([IPython.events]).on('app_initialized.NotebookApp', function(){
    // add here logic that shoudl be run once per **page load**

     $.getScript('/static/components/codemirror/mode/commonlisp/commonlisp.js');

    CodeMirror.requireMode('common-lisp', function(){
        console.log('Lisp mode should now be available in codemirror.');
    })
   IPython.CodeCell.options_default['cm_config']['mode'] = 'commonlisp';
   IPython.CodeCell.options_default['cm_config']['indentUnit'] = 4;

   var cells = IPython.notebook.get_cells();
   for(var i in cells){
       var c = cells[i];
       if (c.cell_type === 'code') {
            // Force the mode to be common lisp
            // This is necessary, otherwise sometimes highlighting just doesn't happen.
            // This may be an IPython bug.
            c.code_mirror.setOption('mode', 'commonlisp');
            c.auto_highlight()
        }
   }

});
document.title = document.title.replace('IPython', 'Fishbowl');
