import re

# Define the input and output file paths
input_file_path = "C:/Users/nikod/Documents/PythonProjects/MRBook/MRbook/imprecision_weight/paper/QuartoAltPaper/ScriptFigureMod/FirstTest.rmd"
output_file_path = "C:/Users/nikod/Documents/PythonProjects/MRBook/MRbook/imprecision_weight/paper/QuartoAltPaper/ScriptFigureMod/AfterFirstTest.rmd"



# Function to convert figure environment to the desired format
def convert_figure(match):
    label = match.group(1).strip()
    caption = match.group(2).strip()
    code_chunk = match.group(3).strip()
    new_syntax = f"""```{{r {label}, echo=FALSE, eval=TRUE, fig.align="center", cache=TRUE, fig.show="hold", out.width="60%", warning=FALSE, message=FALSE}}
#| label: {label}
#| fig-cap: "{caption}"
{code_chunk}
"""
    return new_syntax

with open(input_file_path, 'r') as input_file:
    content = input_file.read()
    # Regular expression to match figure environments in Rmd without [H]
    figure_pattern = r'\begin{figure}HH\n(.*?)\end{figure}\n{r (.*?),.*,.*}\n#| fig-cap: "(.*?)"\n(.*?)'
    # Use re.sub to find and replace figure environments
    
converted_content = re.sub(figure_pattern, convert_figure, content, flags=re.DOTALL)



with open(output_file_path, 'w') as output_file:
    output_file.write(converted_content)

    print("Conversion complete. The output has been saved to", output_file_path)