#!/usr/bin/env bash

cat remark-template.html | replace REPLACE_THIS_WITH_MARKDOWN "$(cat $@)" | bcat
