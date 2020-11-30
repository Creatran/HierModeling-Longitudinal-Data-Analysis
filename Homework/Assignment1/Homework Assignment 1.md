**Homework Assignment 1**

**Due on May 22, 2020** 

1. Coronary bypass surgery is a major surgical procedure that is often performed on older adults with heart conditions. Recovery time from the surgery varies, and is potentially associated with age. The state regulations and hospital practices on the procedure also vary, and may affect the recovery time.

You have a dataset on recovery time. Model recovery time appropriately, and analyze the data with all the statistical tools you have learned, and study what effects recovery time. The data are at the file recovery.csv at the Folder “Datasets” under the “Files” Section.

2. Some Type II Diabetes patients are going through negative life changes, which is likely to raise their blood sugar level.

You have data on A1C (a measure of average blood sugar over months) as the patients go through negative life changes. Build a quadratic growth model based using random effects. The data are at the file A1Cq.csv at the Folder “Datasets” under the “Files” Section.



You need to upload the following (in Canvas) in a single file:

1. Your codes.
2. Output
3. A description (including the linear model equation) of the model you created, and why you created this particular model.
4. A description of the analyses you have performer, and why you performed them.
5. Your conclusions.




The variances of the random effect of the quadratic time is 0. It seems like we do not need to add this random effect in our model. So, I tried the following model without this random  effect. 
$$Y_{ij} = a_i + b_iTime_{ij} + \epsilon_{ijk}$$ 