# 👪👪 Multifamily Households
A multifamily household is one where multiple individual [family units](https://usa.ipums.org/usa-action/variables/NFAMS#description_section), unrelated by blood or marriage, live in same [dwelling unit](https://cps.ipums.org/cps/sample_designs.shtml#:~:text=A%20dwelling%20unit%20is%20a%20room%20or,dwelling%20units%20used%20in%20recent%20U.S.%20censuses.) (e.g. a single house or apartment). Our analysis shows that over 1 in 5 Americans lived in multifamily households in the 1900s, followed by a steep decline through the 20th century until the 1970s, when only 1 in 40 Americans lived in multifamily households. The multifamily share subsequently rose, with roughly 7-8% of Americans living under the same roof as at least one other family as of 2023.

![](output/figures/fig03-multifam-decades-line.jpeg)

*Who* accounts for this change, and what do our findings reveal about the social, economic, and demographic changes in the United States?  We use publicly available data from American Community Survey and Decennial Census, accessed via IPUMS, alongside the analytical tools in the [demographR](https://github.com/lorae/demographr) package, to answer some of these questions.

## ⚡ Quick Start
For experienced users who just want to get the project running right away. If you
have trouble following these steps, please follow the **Detailed Start** guide below.

1. Navigate to the directory where you want the project to be saved and clone both required repos side by side

    ```bash
    cd your/path/to/parent/directory
    ```

    ```bash
    git clone https://github.com/lorae/multifamily-households multifamily-households
    git clone https://github.com/lorae/demographr demographr
    ```

2. Enter the main project

    ```bash
    cd multifamily-households
    ```

3. Copy the environment file and edit it with your own [IPUMS API key](https://account.ipums.org/api_keys)

    ```bash
    cp example.Renviron .Renviron
    # (Windows PowerShell: Copy-Item example.Renviron .Renviron)
    # IMPORTANT: open .Renviron and replace "your_ipums_api_key" with your actual key
    ```

4. Restore dependencies and run the analysis

    Open `multifamily-households.Rproj` in your preferred IDE, then in the R console:
    
    ```r
    renv::restore()
    source("run-all.R")
    ```
    


## 📎 Detailed Start
Detailed instructions for how to fully install and run this project code on your computer.

###  Part A: Clone the repo and configure the R project

These steps will allow you to install the code on your computer that runs this project and set up the environment so that it mimics the environment on which the code was developed.

1. **Clone the repo**: Open a terminal on your computer. Navigate to the directory you would like to be the parent directory of the repo, then clone the repo.

    MacOS/Linux:
    
    ```bash
    cd your/path/to/parent/directory
    ```
    ```bash
    git clone https://github.com/lorae/multifamily-households multifamily-households
    ```
    
    Windows:
    
    ```cmd
    cd your\path\to\parent\directory
    ```
    ```cmd
    git clone https://github.com/lorae/multifamily-households multifamily-households
    ```

2. **Open the R project**: Navigate into the directory, now located at `your/path/to/parent/directory/multifamily-households`.
Open `multifamily-households.Rproj` using your preferred IDE for R. (We use R Studio.)

    Every subsequent time you work with the project code, you should always open the `multifamily-households.Rproj` file
    at the beginning of your work session. This will avoid common issues with broken file paths or an incorrect working directory.

3. **Initialize R environment**: Install all the dependencies (packages) needed to make the code run on your computer.

    First, ensure you have the package manager, `renv`, installed. Run the following in your R console:
    
    ```r
    install.packages("renv") # Safe to run, even if you're not sure if you already have renv
    ```
    ```r
    library("renv")
    ```
    
    Then restore the project:
    
    ```r
    renv::restore()
    ```

4. **Clone the sibling repo, `demographr`**: This project makes use of a bundle of functions that are unit-tested
and generalized in a package called `demographr`. Clone this repo in the same parent directory where you cloned 
`immigrant-households`.

    🛑 Important: Do not clone this **inside** of the `immigrant-households` repo: instead, it should be a 
    sibling: it should contained in the same folder structure as `multifamily-households`.

    MacOS/Linux:
    
    ```bash
    cd your/path/to/parent/directory
    ```
    ```bash
    git clone https://github.com/lorae/demographr demographr
    ```
    
    Windows:
    
    ```cmd
    cd your\path\to\parent\directory
    ```
    ```cmd
    git clone https://github.com/lorae/demographr demographr
    ```
    
###  Part B: Configure API Access

The [IPUMS Terms of Use](https://www.ipums.org/about/terms) precludes us from directly sharing the raw microdata extract, however,
the data used in this analysis is freely available after setting up an IPUMS USA account, and we provide an automated script that 
writes the API call and downloads the exact data used in this analysis. 

5. **Copy the file** `example.Renviron` to a new file named `.Renviron` in the project root directory. 
You can do this manually or use the following terminal commands:

    MacOS/Linux:
    
    ```bash
    cp example.Renviron .Renviron
    ```
    
    Windows (use PowerShell):
    
    ```ps1
    Copy-Item example.Renviron .Renviron
    ```
    
6. **Set up your IPUMS USA API key**: If you don't already have one, set up a free account on 
[IPUMS USA](https://uma.pop.umn.edu/usa/user/new). Use the new account to login to the 
[IPUMS API key](https://account.ipums.org/api_keys) webpage. Copy your API key from this webpage.

7. **Open `.Renviron`** (‼️**not** `example.Renviron`!) and replace `your_ipums_api_key` with your actual key.  Do not include quotation marks. 
R will automatically load `.Renviron` when you start a new session. This keeps your API key private and separate 
from the codebase.

    🛑 Important: `.Renviron` is listed in `.gitignore`, so it will not be tracked or uploaded to GitHub — but `example.Renviron` is tracked, so do not put your actual API key in the example file.

### Part C: Run the analysis scripts

The code for this project is stored in the `src` folder:

- `scripts/`: executable analysis scripts

- `utils/`: accessory modules (functions), subject to unit tests

8. Run all code by sourcing the `run-all.R` script in your R console:

    ```r
    source("run-all.R")
    ```
    
## A note on income encoding

Income is surprisingly difficult to track reliably through the decades. Not only do social programs such as Medicare come to
be during the study period, but also the questions asked around income become more specific and thorough over time. For example, in 1960, 
the `INCTOT` variable we use in this research was the sum of wage income (`INCWAGE`), business and farm income (`INCBUSFM`) and 
other income (`INCOTHER`). By 1990, this list included the original 3 (with business and farm income now split into separate categories)
As well as 4 additional income variables. For the sake of initial tractable findings in this research, we have chosen to use the 
`INCTOT` variable with no subtractions, but in the future we may consider only including the original three variables over time and other
potential variations of income to test robustness of our findings.

Income variables are subject to both top- and bottom-coding, which vary by census year. To ensure comparability, we adjust all 
monetary values to 2023 dollars and harmonize top and bottom codes accordingly. IPUMS provides a CPI99 variable for inflation 
adjustment, but it only extends back to 1962. To include 1960 data, we instead use the Consumer Price Index (CPI-U) series from the 
Federal Reserve Economic Data (FRED), which begins in 1913. We transform this series into 2023-dollar equivalents, with calculations 
and the data series used documented [here](https://fred.stlouisfed.org/graph/?g=1NDUr).

We recode the special missing value (`999999`) as `NA`. Then, after computing inflation factors, we identify which year’s top and 
bottom codes are most restrictive. As shown in the table below, 2023's bottom code of $19,998 is most restrictive, and 1960's top 
code of $257,150 is most restrictive. We inflate / deflate these factors accordingly and those uniform standards to all data.

**Inflation and Coding Reference Table**

| Year | Inflation Factor (2023 $ per $1) | Bottom Code | Bottom Code (2023 $) | Top Code | Top Code (2023 $) |
|------|----------------------------------:|-------------:|---------------------:|----------:|------------------:|
| 1960 | 10.286 | -$9,900  | -$101,831 | $25,000  | $257,150 |
| 1970 | 7.842  | -$9,900  | -$77,636  | $50,000  | $392,100 |
| 1980 | 3.697  | -$9,995  | -$36,951  | $75,000  | $277,275 |
| 1990 | 2.333  | -$19,998 | -$46,655  | $400,000 | $933,200 |
| 2000 | 1.769  | -$20,000 | -$35,380  | $999,998 | $1,768,996 |
| 2006 | 1.511  | -$19,998 | -$30,216  | none     | none     |
| 2011 | 1.355  | -$19,998 | -$27,097  | none     | none     |
| 2016 | 1.269  | -$19,998 | -$25,377  | none     | none     |
| 2021 | 1.125  | -$19,998 | -$22,498  | none     | none     |
| 2023 | 1.000  | -$19,998 | -$19,998  | none     | none     |

**Harmonized Top- and Bottom-Code Table**


| Year | Inflation Factor (2023 $ per $1) | Harmonized Bottom Code (2023 $) | Harmonized Top Code (2023 $) |
|------|----------------------------------:|---------------------:|------------------:|
| 1960 | 10.286 | -$1,944 | $25,000 |
| 1970 | 7.842  | -$2,550  |$32,791 |
| 1980 | 3.697  | -$5,409  | $69,556 |
| 1990 | 2.333  | -$8,572   | $110,223 |
| 2000 | 1.769  | -$11,305  | $145,364 |
| 2006 | 1.511  | -$13,234  | $170,185  |
| 2011 | 1.355  | -$14,759  | $189,779  |
| 2016 | 1.269  | -$15,759  | $202,640  |
| 2021 | 1.125  | -$17,776  | $228,578  |
| 2023 | 1.000  | -$19,998  | $257,150  |


## 📜 License
MIT License (see LICENSE file).

## 📚 Citation
This repository accompanies ongoing research on households and household size. 

For now, please cite as:  
*Multifamily Households: Replication Code and Analysis*. Maintained by Lorae Stojanovic and Peter Hepburn.  
GitHub. https://github.com/lorae/multifamily-households
