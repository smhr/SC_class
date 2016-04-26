##  Markdown to slides is a command line interface to convert markdown documents to an HTML slideshow.
## Basically, Markdown to slides uses remark to convert your markdown documents to HTML slideshows, i.e. that can be viewed in your favorite modern Web browser.

BUILD_DIR := build
#SRC_DIR := src

REMARK := markdown-to-slides

MARKDOWN := $(wildcard *.md)

HTML := $(patsubst %.md,$(BUILD_DIR)/%.html,$(MARKDOWN))

.PHONY: all checkdirs html clean

all: checkdirs $(HTML)

checkdirs: $(BUILD_DIR)

html: checkdirs $(HTML)

$(BUILD_DIR):
	@mkdir -p $@

# generate HTML files
$(BUILD_DIR)/%.html: %.md
	$(REMARK) -d -o $@ $<

clean:
	@rm -rf $(BUILD_DIR)
