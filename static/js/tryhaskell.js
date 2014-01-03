// Main tryhaskell module.
tryhaskell = {};

// A success hook which can be bound and rebound or set as null.
tryhaskell.successHook = null;

// The current page number.
tryhaskell.currentPage = null;

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
        commandValidate:function(line){
            if (line == "") return false;
            else return true;
        },
        commandHandle:function(line,report){
            if(!tryhaskell.preCommandHook(line,report)) {
                tryhaskell.ajaxCommand(line,report);
            }
        },
        autofocus:true,
        animateScroll:true,
        promptHistory:true,
        welcomeMessage:'Type Haskell expressions in here.'
    });
};

// Make an AJAX command to the server with the given line.
tryhaskell.ajaxCommand = function(line,report){
    $.ajax({
        url: '/eval',
        dataType: 'json',
        data: { 'exp': line },
        success: function(result){
            if(result.error){
                report([{msg:result.error,className:'jquery-console-error'}]);
            } else if (result.success){
                if(tryhaskell.successHook != null)
                    tryhaskell.successHook(result.success);
                report([{msg:result.success.value,className:'jquery-console-value'},
                        {msg:':: ' + result.success.type,className:'jquery-console-type'}]);
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

// Handy method.
String.prototype.trim = function() {
    return this.replace(/^[\t ]*(.*)[\t ]*$/,'$1');
};

// Main entry point.
$(function(){
    tryhaskell.makeController();
    tryhaskell.makeGuide();
});
