# Muscadine

Muscadine is a modern web app (inspired by Twitter) for capturing the in-the-moment thoughts of a single user about a variety of specific topics.

As I consume various media or work on various projects, I've noticed that I tend to have a lot of tweet-sized ideas about those subjects. But because those thoughts are separated by lengths of time (since I only have a few hours a week to devote to any particular hobby), my Twitter timeline appears aimless and disconnected.

Thus, Muscadine allows me to associate each microblog entry with a specific media item or project. Microblogs for a specific item can be viewed as a whole
or the entire feed can be intermixed to show thoughts over time while still
associating each one with a particular thing.

## Future Directions

I can imagine automatic cross-posting to Twitter or even implementing the [Mastodon][mastodon] specification to join that federated microblog system.

I also think that a system for publishing longer essay-level non-micro blogs is inevitable, but I want to actually build up a backlog of essays to publish before starting that work.

The publishing interface is a little rough today. One day, I'd like to polish that if only as a treat for myself (since I'm the only that ever uses it). And, if there's any enthusiasm from others, possibly even rework the data model to be multi-user.

[mastodon]: https://joinmastodon.org/

## About the Name

In recent years, I've started choosing wine/grape varietals as project code names.

[Muscadine grapes][wikipedia] are the grapes most common to my region of the United States and are the grapes most often used to produce local wines in this area. It seemed imminently suitable for the code name for my home page since they are, indeed, the grapes of my home.

Additionally, muscadine wine is not the best tasting wine you'll ever find but it has unabashedly local character. I expect my home page to feel much the same.

[wikipedia]: https://en.wikipedia.org/wiki/Vitis_rotundifolia

## Building and Running

I use the [Ionide][ionide] extension in VS Code to build and run this software.

It requires [MongoDB][mongodb]: I run it in a Docker container.

The `appsettings.json` file contains placeholder settings. You can add an `appsettings.Development.json` file to fill those in locally. I set them as environment variables in production.

You will need to _dramatically_ change the `publish_site.yaml` pipeline for your own needs.

You should change the images in the `/img` directory and the `facicon.ico`; they are not included in the license for this software.

[ionide]: https://ionide.io/
[mongodb]: https://www.mongodb.com/