# Quarto Ocean Theme — Usage Guide

## Files

| File | Purpose |
|------|---------|
| `custom.css` | Main stylesheet (typography, colours, tables, code blocks) |
| `header.html` | Top ocean banner (include before body) |
| `footer.html` | Bottom ocean banner + personal details |

## YAML front matter

Add these lines to any `.qmd` document:

```yaml
---
title: "Your Report Title"
author: "Théophile Mouton"
date: today
format:
  html:
    css: custom.css
    include-before-body: header.html
    include-after-body: footer.html
    toc: true
    toc-depth: 3
    number-sections: true
    theme: none        # disables Bootstrap defaults
---
```

Or apply globally in `_quarto.yml` for a whole project:

```yaml
format:
  html:
    css: custom.css
    include-before-body: header.html
    include-after-body: footer.html
    theme: none
```

## Using a real ocean photo

In `custom.css`, find the comment block under `.ocean-header` and replace
the gradient with your image. The easiest way is to add a class to `header.html`:

```html
<div class="ocean-header use-photo">
```

Then in `custom.css` uncomment/add:

```css
.ocean-header.use-photo {
  background-image: url('ocean.jpg');
  background-size: cover;
  background-position: center 60%;
}
.ocean-header.use-photo .ocean-header-overlay {
  background: rgba(6,45,74,0.50);   /* darkens the photo so it reads well */
}
```

Do the same for the footer by adding a `background-image` rule to `.ocean-footer`.

## Customising footer details

Edit `footer.html` directly — the three columns are:

- `.footer-left`   : your name and role
- `.footer-center` : institution and programme
- `.footer-right`  : website and consulting entity

## Colour tokens (easy to change)

All colours are CSS variables at the top of `custom.css`:

```css
:root {
  --ocean-deep:    #062d4a;
  --ocean-mid:     #0d4f7a;
  --ocean-surface: #1a7fa8;
  --ocean-light:   #4eb8d4;
  --ocean-foam:    #c8eaf4;
}
```

Change those six values to shift the entire colour scheme.
