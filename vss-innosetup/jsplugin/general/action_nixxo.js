function fix_sub(tag, acc) {
    var sub = VSSCore.GetFirst();
    var sub_fixed;
    var num = 0;
    while (sub != null) {
        sub_fixed = sub.Text;
        if (tag) {
            sub_fixed = sub_fixed.replace(/< *\/{0,1} *[bigsu] *>/gi, '');
            sub_fixed = sub_fixed.replace(/\{\\*an*\d{1,2}\}/gi, '');
            sub_fixed = sub_fixed.replace(/<\/*font.*?>/gi, '');
        }
        if (acc) {
            // esclusione sta', da', fa', va'
            sub_fixed = sub_fixed.replace(/(([^\s][Ss]t)|([^Ss]t)|([^\s][DdFfVv])|([^DdFfVvt]))a'([\.,-;:"\!\?\s\n\<\>]|$)/g, '$1à$6');
            // -ché
            sub_fixed = sub_fixed.replace(/che'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'ché$1');
            // -tré
            sub_fixed = sub_fixed.replace(/tre'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'tré$1');
            // Né uno né l'altro.
            sub_fixed = sub_fixed.replace(/([\.,-;:"\!\?\s\n\<\>]|^)([Nn])e'/g, '$1$2é');
            // sé
            sub_fixed = sub_fixed.replace(/([\.,-;:"\!\?\s\n\<\>]|^)([Ss])e'/g, '$1$2é');
            // esclusione de'
            sub_fixed = sub_fixed.replace(/(([^\s][Dd])|([^Dd])|^)e'([\.,-;:"\!\?\s\n\<\>]|$)/g, '$1è$4');
            // esclusione di'
            sub_fixed = sub_fixed.replace(/(([^\s][Dd])|([^Dd]))i'([\.,-;:"\!\?\s\n\<\>]|$)/g, '$1ì$4');
            // esclusione po'
            sub_fixed = sub_fixed.replace(/(([^\s][Pp])|([^Pp]))o'([\.,-;:"\!\?\s\n\<\>]|$)/g, '$1ò$4');
            sub_fixed = sub_fixed.replace(/u'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'ù$1');
            sub_fixed = sub_fixed.replace(/A'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'À$1');
            sub_fixed = sub_fixed.replace(/CHE'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'CHÉ$1');
            sub_fixed = sub_fixed.replace(/TRE'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'TRÉ$1');
            sub_fixed = sub_fixed.replace(/([\.,-;:"\!\?\s\n\<\>]|^)([N])E'/g, '$1$2É');
            sub_fixed = sub_fixed.replace(/E'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'È$1');
            sub_fixed = sub_fixed.replace(/I'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'Ì$1');
            sub_fixed = sub_fixed.replace(/O'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'Ò$1');
            sub_fixed = sub_fixed.replace(/U'([\.,-;:"\!\?\s\n\<\>]|$)/g, 'Ù$1');
        }
        if (sub.Text != sub_fixed) {
            sub.Text = sub_fixed;
            num++;
        }
        sub = VSSCore.GetNext(sub);
    }
    if (num > 0) ScriptLog('Correzioni: ' + num);
};

JSAction_remove_tag = {
    onExecute: function() {
        fix_sub(true, false);
    }
};

JSAction_remove_acc = {
    onExecute: function() {
        fix_sub(false, true);
    }
};

JSAction_remove_all = {
    onExecute: function() {
        fix_sub(true, true);
    }
};

VSSCore.RegisterJavascriptAction('JSAction_remove_tag', 'Remove Tags', '');
VSSCore.RegisterJavascriptAction('JSAction_remove_acc', 'Remove Apostrophes', '');
VSSCore.RegisterJavascriptAction('JSAction_remove_all', 'Remove All', '');