using UnityEngine;

public class LookAtCamera : MonoBehaviour
{
    void OnEnable()
    {
        LookAtCameraManager.Add(this);
    }

    void OnDisable()
    {
        LookAtCameraManager.Remove(this);
    }
}