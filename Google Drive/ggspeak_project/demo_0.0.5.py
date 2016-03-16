#!/usr/bin/python

# Title: Demo of ggspeak
# Author: Maurice Diesendruck
# Last updated: 2015-09-01
#
# Graph by voice.

import speech_recognition as sr
import unicodedata
import os
from Graphic_mpl import Graphic
from copy import copy
import pandas as pd
from matplotlib import pyplot as plt
plt.style.use('ggplot')
plt.ion()


def main():
    # Give introduction to program and goal.
    introduction()

    # Get recognizer and microphone objects.
    r, mic = prepare_mic()

    # Instantiate empty graph object.
    g_empty = Graphic()

    # Set dataset and filename values of graph object by choosing dataset.
    g_base = choose_dataset(g_empty)
    g = copy(g_base)

    # Run speech recognition and graphing in a streaming format.
    attentive = True
    while attentive:
        raw_input('Tap ENTER to continue.')
        with mic as source:
            audio = get_audio(r, source)
            try:
                text = r.recognize(audio)
                print('You said: ' + text)
            except LookupError:
                print("Didn't get audio.")
                continue
            # See if command is quit, save, reset, or edit.
            if text:
                terms = tokenize(text)

                # Decide what the terms indicate, and do the actions.
                if is_quit(terms):
                    print 'Goodbye'
                    return None
                elif is_save(terms):
                    # TODO: Write save function.
                    continue
                elif is_reset(terms):
                    print 'Define a new graph.'
                    g = copy(g_base)
                    plt.clf()
                elif g.has_base():
                    g = update_graph(g, terms)
                else:
                    g = create_graph(g, terms)

                # Create new plot with current set of object attributes.
                g.make_gg_plot()


def create_graph(g, terms):
    """With terms, extracts basic graph elements.

    Gets data columns and geometry of graph.

    Args:
        g: Graphic object.
        terms: Terms from latest voice command.

    Returns:
        g: Graphic object.
    """
    # Search the command for instructions about specific graph attributes.
    g = extract_data_cols(g, terms)
    g = extract_geom(g, terms)
    return g


def update_graph(g, terms):
    """With terms, edits ancillary graph features, like titles and labels.

    Determines ancillary features, like titles, labels, smoothing functions,
    groupings, stackings, etc.

    Args:
        g: Graphic object.
        terms: Tokens from latest voice command.

    Returns:
        g: Graphic object.
    """
    g = extract_stat_functions(g, terms)
    return g


def introduction():
    print('\n\n----------- GGSPEAK: Graph by Voice ------------')


def prepare_mic():
    """Instantiates recognizer and microphone objects.

    These objects come from the speech_recognition package.

    Args:
    NA

    Returns:
    r: A recognizer object.
    m: A microphone object.
    """
    r = sr.Recognizer()
    m = sr.Microphone()
    with m as source:
        r.adjust_for_ambient_noise(source, duration=2)
        r.pause_threshold = 1.5
    return r, m


def choose_dataset(g):
    print 'Type file name.'
    filename = os.getcwd()+'/'+raw_input('Filename: '+os.getcwd()+'/')
    try:
        dataset = pd.read_csv(filename)
    except LookupError:
        print("No document found.")

    # Set dataset as the chosen file.
    g.dataset = dataset
    g.filename = filename

    names = '[' + ', '.join(list(g.dataset.columns.values)) + ']'
    names = names.upper()
    print('\nYou are using the dataset ' + g.filename)
    print('Data preview:')
    print(g.dataset.head(5))
    print

    return g


def get_audio(r, source):
    print "\n Listening..."
    audio = r.listen(source)
    return audio


def tokenize(text):
    text = unicodedata.normalize('NFKD', text).encode('ascii', 'ignore')
    text = text.lower()
    terms = text.split(' ')
    return terms


def is_quit(terms):
    quit_words = [
        'quit', 'stop', 'done', 'finish', 'finished', 'end', 'enough']
    wants_to_quit = bool(set(quit_words) & set(terms))
    return wants_to_quit


def is_reset(terms):
    reset_words = ['reset', 'clear']
    wants_to_reset = bool(set(reset_words) & set(terms))
    return wants_to_reset


def is_save(terms):
    save_words = ['save']
    wants_to_save = bool(set(save_words) & set(terms))
    return wants_to_save


def extract_data_cols(g, terms):
    try:
        g.data_cols = [t for t in terms if t in g.dataset.columns.values]
        print('Relevant variables: ' + str(g.data_cols))
    except:
        print('Did not catch any matching variable names.')
    return g


def extract_geom(g, terms):
    if 'histogram' in terms:
        g.geom = 'histogram'
    elif 'bar' in terms or 'barplot' in terms:
        g.geom = 'bar'
    elif 'density' in terms:
        g.geom = 'density'
    elif 'line' in terms:
        g.geom = 'line'
    elif 'point' in terms or 'scatter' in terms:
        g.geom = 'point'
    elif len(g.data_cols) == 1:
        g.geom = 'histogram'
    elif len(g.data_cols) == 2:
        g.geom = 'point'
    else:
        print("Couldn't identify geometry of graph.")
    return g


def extract_stat_functions(g, terms):
    if 'smooth' in terms:
        g.add_smooth = True
    return g


if __name__ == "__main__":
    main()
