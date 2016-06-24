from lxml import html
import requests
import string

list_name_candidates = list()
num_speech = 0
id_candidates_speech = list()
time_stamp_list = list()
type_text_list = list()
with open('speech_list.csv', 'r') as f_list:
    all_url_strings = f_list.readlines()
    for url_string in all_url_strings:
            page = requests.get(url_string.split('\t')[3])
            type_text = url_string.split('\t')[2]
            complete_candidate_name = url_string.split('\t')[0]
            time_stamp = url_string.split('\t')[1]
            if (not (complete_candidate_name in list_name_candidates)):
                list_name_candidates.append(complete_candidate_name)
                id_candidate = len(list_name_candidates) - 1
            else:
                id_candidate = list_name_candidates.index(complete_candidate_name)
            id_candidates_speech.append(str(id_candidate))
            type_text_list.append(type_text)
            time_stamp_list.append(time_stamp)

            tree = html.fromstring(page.content)


            if ((type_text=='Remarks') or (type_text=='Video') or (type_text=='Interview')):
                name_candidate = complete_candidate_name.split(' ')[1]
                # More Complete version:
                all_elems = tree.xpath('//span[@class="displaytext"]/child::node()')
                all_text = ""
                skip = 0
                for elem in all_elems:
                    if (isinstance(elem, html.HtmlElement)):
                        if (elem.tag == 'p'):
                            break
                        exclude = set(string.punctuation)
                        if (elem.text is not None):
                            current_speaker_name = ''.join(ch for ch in elem.text.lower() if ch not in exclude)
                            if (current_speaker_name != name_candidate.lower()):
                                skip = 1
                                continue
                            else:
                                skip = 0
                    else:
                        if (skip == 0):
                            all_text += elem

                all_text += '\n'

                my_elements = tree.xpath('//span[@class="displaytext"]//p')
                skip = 0
                for elem in my_elements:
                    check = type(elem)
                    found = 0
                    if (isinstance(elem, html.HtmlElement)):
                        current_text = elem.xpath('.//text()')
                        current_children = elem.xpath('./child::node()')
                        if (len(current_children) >= 2):
                            if (isinstance(current_children[1], html.HtmlElement)):
                                exclude = set(string.punctuation)
                                if (current_children[1].text is not None):
                                    current_speaker_name = ''.join(
                                        ch for ch in current_children[1].text.lower() if ch not in exclude)
                                    if (current_speaker_name != name_candidate.lower()):
                                        found = 0
                                        skip = 1
                                        continue
                                    else:
                                        found = 1
                                        skip = 0

                        if ((isinstance(current_children[0], html.HtmlElement)) and (found == 0)):
                            exclude = set(string.punctuation)

                            if (current_children[0].text is not None):
                                current_speaker_name = ''.join(
                                    ch for ch in current_children[0].text.lower() if ch not in exclude)
                                if (current_speaker_name != name_candidate.lower()):
                                    skip = 1
                                    continue
                                else:
                                    skip = 0

                        if (skip == 0):
                            for sentence in current_text:
                                all_text = all_text + " " + sentence
                            all_text += '\n'
            if (all_text=='\n'):
                all_text=''
            if (not((type_text == 'Remarks') or (type_text == 'Video') or (type_text == 'Interview'))) or (all_text==''): #if it is not a discussion or the previous algorithm failed
                #More Complete version:
                all_elems = tree.xpath('//span[@class="displaytext"]//child::node()')
                all_text = ""
                for elem in all_elems:
                    if (isinstance(elem, html.HtmlElement)):
                        if (elem.tag=='p'):
                            break
                    else:
                        all_text += elem

                all_text += '\n'

                my_elements = tree.xpath('//span[@class="displaytext"]//p')

                for elem in my_elements:
                    check = type(elem)
                    if (isinstance(elem, html.HtmlElement)):
                        current_text = elem.xpath('.//text()')
                        for sentence in current_text:
                            all_text = all_text + " " + sentence
                        all_text += '\n'
            all_text = all_text.lower()
            #exclude = set(string.punctuation)
            #all_text = ''.join(ch for ch in all_text if ch not in exclude)
            for part_name in complete_candidate_name.split(' '):
                all_text = all_text.replace(part_name.lower(), "")
            print 'Done %d/%d' % (num_speech+1, len(all_url_strings))

            with open('RemarksAndStatement/text_files/speech_example_%d.txt' % num_speech, 'w') as f:
                f.write(all_text.encode('utf-8'))
            num_speech += 1

with open('RemarksAndStatement/id_candidates_speech.txt', 'w') as f:
    for current_id in id_candidates_speech:
        f.write(current_id+"\n")

with open('RemarksAndStatement/name_candidates_speech.txt', 'w') as f:
     for name_candidate in list_name_candidates:
        f.write(name_candidate + "\n")

with open('RemarksAndStatement/type_texts.txt', 'w') as f:
    for type_text in type_text_list:
        f.write(type_text + "\n")

with open('RemarksAndStatement/timestamp_candidates_speech.txt', 'w') as f:
    for time_stamp in time_stamp_list:
        f.write(time_stamp+"\n")