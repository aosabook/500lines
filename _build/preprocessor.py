import re

def remove_tags(tagname, text):
    """Use a regular expression
    to remove parts of a chapter that shouldn't be in the target
    format.
    """
    regex = re.compile(r'<' + tagname + r'>(.|\s+)*?</' + tagname + r'>', flags=re.MULTILINE)
    return regex.sub('', text)

def strip_tags(tagname, text):
    """Use a regular expression to remove tags from
    the parts of the chapter that *should* be in the target format.
    """
    regex = re.compile(r'<' + tagname + r'>((.|\s+)*?)</' + tagname + r'>', flags=re.MULTILINE)
    return regex.sub(r'\1', text)

def root_paths(text):
    return re.sub(r'\(([^()]*?g)\)', r'(/\1)', text)

def parse_aosafigures(text):
    '''parse_aosafigures(text) -> [(fullmatch, width, location, caption, id)]'''
    # \aosafigure[233pt]{warp-images/2.png}{Event driven}{fig.warp.f2}
    regex = re.compile("(\\\\aosafigure\[?([^\]]+)?\]?\{(.*)\}\{(.*)\}\{(.*)\})",re.MULTILINE)
    figures_list = regex.findall(text)
    return figures_list

def test_parse_aosafigures():
    test = (
        ('''\\aosafigure[240pt]{bla}{bla}{bla}

\\aosafigure{bla}{bla}{bla}
''', [('\\aosafigure[240pt]{bla}{bla}{bla}', '240pt', 'bla', 'bla', 'bla'), ('\\aosafigure{bla}{bla}{bla}', '', 'bla', 'bla', 'bla')]),)

    for input_text, expected in test:
        aosafigs = parse_aosafigures(input_text)
        print aosafigs
        print
        print 'expected'
        print expected
        assert aosafigs == expected

def fix_figure_index(text, chapter):
    """
    Creates figures references.
    """
    figures_list = parse_aosafigures(text)
    figures = []
    index = 1
    for figure in figures_list:
        # regex tokens
        figures.append(figure)
        width = figure[1]
        location = figure[2]
        label = figure[3]
        ref = figure[4]
        
        # the original line, to be replaced by the image in the HTML version 
        # plus the <a> label
        original_aosafigure = figure[0]
        fignum = "{chapter}.{index}".format(chapter=chapter, index=str(index))
        html_figure_text = '<div class="center figure"><a name="figure-' + fignum + '"></a><img src="' + location + '" alt="Figure ' + fignum + ' - ' + label + '" title="Figure ' + fignum + ' - ' + label + '" /></div><p class="center figcaption"><small>Figure ' + fignum + ' - ' + label + '</small></p>'
        text = text.replace(original_aosafigure, html_figure_text)
        
        # we want to replace the reference
        to_replace = '\\aosafigref{'+ref+'}'
        # with a nice phrase
        new_value = '<a href="#figure-' + fignum + '">Figure ' + fignum + '</a>' 
        text = text.replace(to_replace, new_value)
        
        index += 1
    return text

def fix_table_references(text, chapter):
    """
    Creates table references.
    """
    
    # Find table captions with :\s\label{$VAR}
    tables = list()
    table_caption_regex = re.compile('^:\s\\\\label\{([a-zA-Z0-9\.\~\!\?]*)\}', re.MULTILINE)
    tables_found = table_caption_regex.findall(text)
    for caption in tables_found:
        tables.append(caption)
            
    table_count = 1
    for table in tables:
        print '\\aosatblref{'+table+'}'
        table_id = "{}.{}".format(chapter, table_count)
        text = text.replace('\\label{'+table+'}', '<b>Table ' + table_id + '</b> - ')
        text = text.replace('\\aosatblref{'+table+'}', 'Table ' + table_id)
        table_count += 1
        
    return text

def fix_section_references(text):
    """
    Creates section references.
    """
    
    # Find titles before \label
    titles = {}
    text = text.replace('\r\n', '\n')
    lines = text.split('\n')
    last_line = None
    title_regex = re.compile('^#+(.*)$')
    label_regex = re.compile('\\\\label\{([a-zA-Z0-9\.\~\!\?]*)\}')
    for line in lines:
        line = line.strip(' \t\n\r')
        found_label = label_regex.findall(line)
        if (len(found_label) > 0 and last_line != None):
            le_label = found_label[0].strip(' \t\n\r')
            found_titles = title_regex.findall(last_line)
            if (len(found_titles) > 0):
                le_title = found_titles[0].strip(' \t\n\r')
                titles[le_label] = le_title
        if line != '':
            last_line = line
    
    #print titles
    
    # the text is in \aosasecref{sec.warp.arch} and ...
    # \label{sec.warp.arch}
    # Create an associative array with each reference and its index
    regex = re.compile("(\\\\aosasecref\{([a-zA-Z0-9\.\~\!\?]*)\})",re.MULTILINE)
    references_list = regex.findall(text)
    references = []
    index = 1
    for reference in references_list:
        # regex tokens
        references.append(reference)
        label = reference[1]
        
        # the reference text to be replaced by a <a href='#name'> link 
        # print 'label: ' + titles[label]
        html_reference_text = '<a href="#' + str(label) + '">'+titles[label]+'</a>'
        text = text.replace(reference[0], html_reference_text)
        
        # we want to replace the reference
        to_replace = '\\label{'+label+'}'
        # with the <a name=''> link
        new_value = '<a name="' + str(label) + '">&nbsp;</a>' 
        text = text.replace(to_replace, new_value)
        
        index += 1
    return text

def test_root_paths():
    test = '''
burble burble burble

[hey (there)](ethercalc-images/ethercalc.png)

burble burble burble some more'''
    expected = '''
burble burble burble

[hey (there)](/ethercalc-images/ethercalc.png)

burble burble burble some more'''
    result = root_paths(test)
    assert result == expected


def htmlify_refs(text):
    # Double square brackets because pandoc renders only the number.
    pat = r'\\cite\{([a-zA-Z0-9+\-:_]+)\}'
    regex = re.compile(pat)
    return regex.sub('[[@\\1]]', text)


def test_htmlify_refs():
    test = '''
dsaf asd \\cite{bla-99} fas \\cite{o:o_f23} df \\cite{bla-99}.
'''
    # Double square brackets because pandoc renders only the number.
    expected = '''
dsaf asd [[@bla-99]] fas [[@o:o_f23]] df [[@bla-99]].
'''
    print htmlify_refs(test)
    assert htmlify_refs(test) == expected


if __name__ == '__main__':
    import sys
    import argparse
    parser = argparse.ArgumentParser(description="Remove the specified kind of tag from the input document")
    parser.add_argument('doc', nargs='*')
    parser.add_argument('--output', dest='output', required=True)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--latex', action='store_true', default=False)
    group.add_argument('--markdown', action='store_true', default=False)
    parser.add_argument('--chapter', type=int, default=0)
    parser.add_argument('--html-paths', action='store_true', default=False)
    parser.add_argument('--html-refs', action='store_true', default=False)
    args = parser.parse_args()
    destination_file = open(args.output, 'w')
    if len(args.doc) > 0:
        input_file = open(args.doc[0])
    else:
        input_file = sys.stdin
    input_document = input_file.read()
    if args.markdown:
        tag_name = 'markdown'
        other_tag_name = 'latex'
    else:
        tag_name = 'latex'
        other_tag_name = 'markdown'
        input_document = fix_figure_index(input_document, args.chapter)
        input_document = fix_section_references(input_document)
        input_document = fix_table_references(input_document, args.chapter)
        if args.html_refs:
            input_document = htmlify_refs(input_document)
    out = input_document
    out = remove_tags(tag_name, out)
    out = strip_tags(other_tag_name, out)
    if args.html_paths:
        out = root_paths(out)
    destination_file.write(out)
