// Main tryhaskell module.
tryhaskell = {};

// A success hook which can be bound and rebound or set as null.
tryhaskell.successHook = null;

// The current page number.
tryhaskell.currentPage = null;

// Stdout state from the current IO evaluation.
tryhaskell.stdout = [];

// Stdin state for the current IO evaluation.
tryhaskell.stdin = [];

// IO expression.
tryhaskell.io = null;

// Files in the file system.
tryhaskell.files = {
    "/hello": "Hello, World!",
    "/files": "Your file system changes will stick around in your browser's local storage!",
    "/welcome": "Welcome to your mini filesystem! Try playing with this function: getDirectoryContents",
    "/functions": "You can also check out removeFile, writeFile, appendFile"
};

try {
    if(typeof(Storage)!=="undefined")
    {
        tryhaskell.files = (localStorage.files && JSON.parse(localStorage.files))
            || tryhaskell.files;
    };
} catch (e){ tryhaskell.files = {} }

// A pre-command hook which can prevent the command from being run if
// it returns true.
tryhaskell.preCommandHook = function(line,report){
    var m, pages = tryhaskell.pages.list;
    if (m = line.trim().match(/^step([0-9]+)/)) {
        var n = m[1] * 1;
        if (n <= pages.length) {
            tryhaskell.setPage(n,null);
            report();
            return true;
        }
    }
    else if (m = line.trim().match(/^lesson([0-9]+)/)) {
        var n = m[1] * 1;
        for (var i = 0; i < pages.length; i++) {
            if (pages[i].lesson == n) {
                tryhaskell.setPage(i,null);
                report();
                return true;
            }
        }
    } else if (line.trim() == 'next') {
        if (tryhaskell.currentPage < tryhaskell.pages.list.length) {
            tryhaskell.setPage(tryhaskell.currentPage + 1);
        }
        report();
        return true;
    } else if (line.trim() == 'back') {
        if (tryhaskell.currentPage > 1) {
            tryhaskell.setPage(tryhaskell.currentPage - 1);
        }
        report();
        return true;
    } else if (line.trim() == 'help') {
        tryhaskell.setPage(2,null);
        report();
        return true;
    }
    return false;
};

// Make the console controller.
tryhaskell.makeController = function(){
    tryhaskell.controller = $('#console').console({
        promptLabel: 'λ ',
        commandValidate: function(line){
            if (line == "") return false;
            else return true;
        },
        commandHandle: function(line,report){
            if(tryhaskell.io === null){
                if(!tryhaskell.preCommandHook(line,report)){
                    tryhaskell.ajaxCommand(line,report,[]);
                }
            } else {
                tryhaskell.stdin.push(line);
                tryhaskell.ajaxCommand(tryhaskell.io,report,tryhaskell.stdin);
            }
        },
        autofocus: true,
        animateScroll: true,
        promptHistory: true,
        welcomeMessage: 'Type Haskell expressions in here.',
        continuedPromptLabel: '> '
    });
};

// Make an AJAX command to the server with the given line.
tryhaskell.ajaxCommand = function(line,report,stdin){
    var args = { 'exp': line,
                 'args': JSON.stringify([stdin,tryhaskell.files])
               };
    $.ajax({
        url: '/eval',
        dataType: 'json',
        type: 'POST',
        data: args,
        success: function(result){
            if(result.stdout !== undefined){
                tryhaskell.files = result.files;
                result = result.stdout;
                tryhaskell.io = line;
                var msgs = [];
                if(result != null){
                    for(var i = tryhaskell.stdout.length; i < result.length; i++) {
                        msgs.push({ msg: result[i], className: 'jquery-console-stdout' });
                    }
                }
                tryhaskell.stdout = result;
                tryhaskell.controller.continuedPrompt = true;
                report(msgs);
                tryhaskell.controller.continuedPrompt = false;
            } else {
                if(result.error !== undefined){
                    result = result.error;
                    report([{ msg: result || 'Unspecified error. Have you installed mueval?',
                              className:'jquery-console-error' }]);
                } else if(result.success){
                    result = result.success;
                    var msgs = [];
                    for(var i = tryhaskell.stdout.length; i < result.stdout.length; i++) {
                        msgs.push({ msg: result.stdout[i], className: 'jquery-console-stdout' });
                    }
                    if(tryhaskell.successHook != null)
                        tryhaskell.successHook(result);
                    if(result.type !== 'IO ()' && !result.value.match(/^</))
                        msgs.push({ msg: result.value, className: 'jquery-console-value' });
                    msgs.push({ msg: ':: ' + result.type, className: 'jquery-console-type' });
                    report(msgs);
                    tryhaskell.files = result.files;
                }
                tryhaskell.io = null;
                tryhaskell.stdout = [];
                tryhaskell.stdin = [];
            }
            if(typeof(Storage)!=="undefined")
            {
                localStorage.files = JSON.stringify(tryhaskell.files);
            }
        }
    });
};

// Make the guide on the rhs.
tryhaskell.makeGuide = function(){
    var match = window.location.href.match(/#step([0-9]+)$/);
    if(match){
        tryhaskell.setPage(match[1]*1,null);
    } else {
        tryhaskell.setPage(1,null);
    }
};

// Set the current page.
tryhaskell.setPage = function(n,result){
    var page = tryhaskell.pages.list[n-1];
    if(page){
        // Update the current page content
        var guide = $('#guide');
        guide.html(typeof page.guide == 'string'? page.guide : page.guide(result));
        tryhaskell.makeGuidSamplesClickable();
        // Update the location anchor
        if (tryhaskell.currentPage != null)
            window.location = '/#step' + n;
        tryhaskell.currentPage = n;
        // Setup a hook for the next page
        var nextPage = tryhaskell.pages.list[n];
        if(nextPage) {
            tryhaskell.successHook = function(result){
                if (nextPage.trigger &&
                    nextPage.trigger(result))
                    tryhaskell.setPage(n+1,result);
            };
        }
    } else {
        throw "Unknown page number: " + n;
    }
};

// Make the code examples in the guide clickable so that they're
// inserted into the console.
tryhaskell.makeGuidSamplesClickable = function() {
    $('#guide code').each(function(){
        $(this).css('cursor','pointer');
        $(this).attr('title','Click me to insert "' +
                     $(this).text() + '" into the console.');
        $(this).click(function(){
            tryhaskell.controller.promptText($(this).text());
            tryhaskell.controller.inner.click();
        });
    });
}

// Display the currently active users
tryhaskell.activeUsers = function(){
    var active = $('.active-users');
    // Tomorrow theme
    var colors =
        [// Tomorrow theme
          "#f5871f" // Orange
         ,"#eab700" // Yellow
         ,"#718c00" // Green
         ,"#3e999f" // Aqua
         ,"#4271ae" // Blue
         ,"#8959a8" // Purple
         // Solarized theme
         ,"#073642" // base02
         ,"#586e75" // base01
         ,"#b58900" // yellow
         ,"#cb4b16" // orange
         ,"#dc322f" // red
         ,"#d33682" // magenta
         ,"#6c71c4" // violet
         ,"#268bd2" // blue
         ,"#2aa198" // cyan
         ,"#859900" // green
        ]
    var color_index = 0;
    var color_cache = {};
    function update(){
        if(!$('.active-users').is(':visible')) return;
        $.get('/users',function(users){
            users = JSON.parse(users);
            $('.active-users .user').remove();
            var color;
            for(var i = 0; i < users.length; i++){
                if(typeof color_cache[users[i][0].toString()] != 'number') {
                    color_cache[users[i][0].toString()] = color_index;
                    color_index++;
                }
                color = colors[color_cache[users[i][0].toString()] % colors.length];
                if (!color) color = colors[5];
                active.append($('<div class="user"></div>').css('background-color',color));
            }
        });
    }
//    setInterval(update,5000);
    update();
};

// Handy method.
String.prototype.trim = function() {
    return this.replace(/^[\t ]*(.*)[\t ]*$/,'$1');
};

// Main entry point.
$(function(){
    tryhaskell.makeController();
    tryhaskell.makeGuide();
    tryhaskell.activeUsers();
});
