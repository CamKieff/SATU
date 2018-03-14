## Star Trek Neural Networks

### Scraping
As it's name implies `episodeScrape.py` scrapes the episodes off of the Star Trek wiki [Memory-Alpha](http://memory-alpha.wikia.com/wiki/Portal:Main). It takes the file `allEpisodes.csv` as input, which is a list of webpage extensions corresponding to every Star Trek: The Next Generation, Deep Space 9, and Voyager episode. The output is `alloutput.txt` as you might imagine. That data was hand-edited to remove the key-word list from each episode. Otherwise the model learns to repeat random words separated by semi-colons. Some out-of-universe show notes were also removed on an _ad hoc_ basis. Edited output found in `alloutput_edit.txt`.

### Neural Network

The network was run using [tensorflow-char-rnn](https://github.com/crazydonkey200/tensorflow-char-rnn), a [Tensorflow](https://www.tensorflow.org/) implementation of Andrej Karpathy's [char-rnn](https://github.com/karpathy/char-rnn). I used the default model parameters for my initial runs. Model is a Long-Short Term Memory (LSTM) network with 2 layers, 128 hidden nodes per layer, 50 training epochs, batch size of 20.  Output temperature defaults to 1, but I also varried it from 0.5 - 1.5.  

It tooks ~24 hours to train the model on my Surface Pro 3 running a virtual LUbuntu environment.  This is probably the least efficient way to train a NN.
